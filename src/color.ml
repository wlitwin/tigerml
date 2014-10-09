module type T =
    functor (Frame : Frame.Frame) ->
sig
    module Graph = Graph.Graph

    type allocation = Frame.register Temp.ITable.table
    type data = {interference : Liveness.igraph;
                 initial: allocation;
                 spillCost: Graph.node -> int;
                 registers: Frame.register list}
    val color : data -> allocation * Temp.temp list
end

module Make : T = functor (Frame : Frame.Frame) ->
struct 
    module Graph = Graph.Graph

    type allocation = Frame.register Temp.ITable.table
    type data = {interference : Liveness.igraph;
                 initial: allocation;
                 spillCost: Graph.node -> int;
                 registers: Frame.register list}

    module LGI = Liveness.Graph.ITable
    module LG = Liveness.Graph
    module TI = Temp.ITable

    let moveList : Temp.temp LGI.table = LGI.empty ()

    module Sadj = Set.Make(struct type t = LG.node * LG.node let compare = compare end)
    module SN   = Set.Make(struct type t = LG.node           let compare = compare end)
    module SI   = Set.Make(struct type t = int               let compare = compare end)

    let simplifyWorklist : LG.node list ref = ref []
    let freezeWorklist   : LG.node list ref = ref []
    let spillWorklist    : SN.t ref = ref SN.empty
    let coalescedNodes   : LG.node list ref = ref []
    let coloredNodes     : LG.node list ref = ref []
    let selectStack      : LG.node Stack.t = Stack.create ()

    (* coalescedMoves
     * constrainedMoves
     * frozenMoves
     * worklistMoves
     * activeMoves
     *)

    type move = LG.node * LG.node
    type color = int

    let adjSet   : Sadj.t ref = ref Sadj.empty
    let adjList  : SN.t TI.table = TI.empty ()
    let degree   : int LGI.table = LGI.empty ()
    let moveList : move list LGI.table = LGI.empty ()
    let alias : LG.node LGI.table = LGI.empty ()
    let color : color LGI.table = LGI.empty ()

    let numColors = 4

    let get func table value = 
        match func (table, value) with
        | Some s -> s
        | None -> raise (Failure "Value not found")
    ;;

    let getTI  table value = get TI.look
    let getLGI table value = LGI.look

    let build (igraph, liveOut, fgraph : Liveness.igraph * (Flowgraph.FGraph.node -> Temp.temp list) * Flowgraph.flowgraph) =
        let open Liveness in
        let blocks = LG.nodes igraph.graph in
        (* forall blocks in program *)
        List.iter (fun block ->
            let live = ref (liveOut block) in
            (* forall instructions(block) in reverse order *)
            if isMoveInstruction(i) then (
                live := live - use(i);
                (* forall n in def(i) U use(i) *)
                let nDefUse = union def(i) use(i) in
                List.iter (fun n ->
                    moveList[n] <- moveList[n] U {i}
                ) nDefUse;
                worklistMoves <- worklistMoves U {i}
            );
            live <- live U def(i);
            forall d in def(i)
                forall l in live
                    AddEdge (l, d)
            live <- use(i) U (live - def(i))
        ) blocks;
    ;;

    let makeWorklist initial =
        let copy = !initial in
        SN.iter (fun n ->
            initial := SN.remove n !initial; 
            if (getLGI degree n) >= numColors then
                spillWorklist := SN.add !spillWorklist n
            else if moveRelated n then
                freezeWorklist := SN.add !freezeWorklist n
            else 
                simplifyWorklist := SN.add !simplifyWorklist n
        ) copy 
    ;;

    let stackToSet stack =
        let set = ref SN.empty in
        Stack.iter (fun elt ->
            set := SN.add !set elt
        ) stack;
        !set
    ;;

    let moveEltFromTo set1 set2 elt =
        set1 := SN.remove !set1 elt;
        set2 := SN.add !set2 elt
    ;;

    let adjacent n =
        SN.diff (getTI adjList n) (SN.union (stackToSet selectStack) !coalescedNodes)
    ;;

    let nodeMoves n =
        SN.diff (getTI moveList n) (SN.union !activeMoves !worklistMoves)
    ;;

    let moveRelated n =
        not SN.empty (nodeMoves n)
    ;; 

    let simplify () =
        let n = SN.choose !simplifyWorklist in
        simplifyWorklist := SN.remove !simplifyWorklist;
        Stack.push selectStack n;
        SN.iter (fun m ->
            decrementDegree m
        ) (adjacent n)
    ;;

    let decrementDegree m =
        let d = getLGI degree m in
        LGI.enter (degree, m, d-1);
        if d = numColors then (
            enableMoves (SN.add (adjacent m) m);
            spillWorklist := SN.add !spillWorklist m;
            if moveRelated m then
                freezeWorklist := SN.add !freezeWorklist m
            else
                simplifyWorklist := SN.add !simplifyWorklist m
        )
    ;;

    let enableMoves nodes =
        SN.iter (fun n ->
            SN.iter (fun m ->
                if SN.mem !activeMoves m then
                    moveEltFromTo activeMoves worklistMoves m
            ) (nodeMoves n)
        ) nodes
    ;;

    let coalesce () =
        let m (=copy(x,y)) = first worklistMoves in
        x <- GetAlias x
        y <- GetAlias y
        let (u, v) =
            if y in precolored then (y, x)
            else (x, y)
        in
        worklistMoves <- worklistMoves - {m}
        if (u = v) then (
            coalescedMoves <- coalescedMoves U {m}
            AddWorklist u
        ) else if v in precolored || (u, v) in adjSet then (
            constrainedMoves <- constrainedMoves U {m}
            AddWorklist u
            AddWorklist v
        ) else if (u in precolored) && 
                  (forall t in Adjacent v, OK(t, u)) &&
                  (u not in precolored) && 
                  (Conservative(Adjacent(u) U Adjacent(v))) 
        then (
            coalescedMoves <- coalescedMoves U {m}
            Combine (u, v)
            Addworklist u
        ) else (
            activeMoves <- activeMoves U {m}
        )
    ;;

    let addWorkList u =
        if (u not in precolored && not(MoveRelated(u)) && degree[u] < K) then (
            freezeWorklist <- freezeWorklist - {u}
            simplifyWorklist <- simplifyWorklist U {u}
        )
    ;;

    let ok (t, y) =
        degree[t] < K || t in precolored || (t, r) in adjSet
    ;;

    let conservative nodes =
        let k = ref 0 in
        SN.iter (fun n ->
            if getLGI degree n >= numColors then
                k := k + 1
        ) nodes;
        !k < numColors
    ;;

    let getAlias n =
        if SN.mem !coalescedNodes n then 
            getAlias (getLGI alias n)
        else n
    ;;

    let combine (u, v) =
        if SN.mem !freezeWorklist v then (
            freezeWorklist := SN.remove !freezeWorklist v
        ) else (
            spillWorklist := SN.remove !spillWorklist v
        );
        coalescedNodes := SN.add !coalescedNodes v;
        LGI.enter (alias, v, u);
        let nodesU = getLGI nodeMoves u
        and nodesV = getLGI nodeMoves v in
        LGI.enter (nodeMoves, u, SN.union nodesU nodesV);
        SN.iter (fun t ->
            addEdge (t, u);
            decrementDegree t
        ) (adjacent v);
        if (getLGI degree u) >= numColors && SN.mem !freezeWorklist u then
            moveEltFromTo freezeWorklist spillWorklist u
    ;;

    let freeze () =
        let u = SN.choose !freezeWorklist in
        moveEltFromTo freezeWorklist simplifyWorklist u;
        freezeMoves u
    ;;

    let freezeMoves u =
        List.iter (fun (x, y) ->
            let v = if (getAlias y) = (getAlias u) 
                then getAlias x 
                else getAlias y
            in
            moveEltFromTo activeMoves frozenMoves m;
            if SN.is_empty (nodeMoves v) && degree[v] < K then (
                moveEltFromTo freezeWorklist simplifyWorklist v 
            )
        ) (nodeMoves u)
    ;;

    let selectSpill () =
        let m = SN.choose !spillWorklist (* Using favorite heuristic *) in
        moveEltFromTo spillWorklist simplifyWorklist m
        freezeMoves m
    ;;

    let assignColors () =
        while Stack.is_empty selectStack do
            let n = Stack.pop selectStack in
            let okColors = ref SI.empty in
            for k = 0 to numColors-1 do
                okColors := SI.add okColors k
            done;
            let okColors =
                List.fold_left (fun w ->
                    if getAlias w 
                ) !okColors (get adjList n)
            in
            forall w in adjList[n]
                if GetAlias(w) in (coloredNodes U precolored) then
                    okColors <- okColors - (color[GetAlias(w)])
            if okColors = [] then
                spilledNodes <- spilledNodes U {n}
            else (
                coloredNodes <- coloredNodes U {n}
                let c = first okColors
                color[n] <- c
            )
        done
        forall n in coalescedNodes
            color[n] <- color[GetAlias(n)]
    ;;

    let rewriteProgram () =
        (* Allocate memory locations for each v in spilledNodes *)
        (* Create a new temporary vi for each definition and each use *)
        (* In the porgram (instructions), insert a store after each definition of a
         * vi, a fetch before each use of vi. *)
        (* Put all the vi into a set newTemps *)
        spilledNodes <- {}
        initial <- coloredNodes U coalescedNodes U newTemps
        coloredNodes <- {}
        coalescedNodes <- {}
        (* Can be made faster by not throwing away all coalescedNodes.
         * Keep all the coalesces found before the first call to SelectSpill *)
    ;;

    let addEdge (u, v) =
        if not (Sadj.mem (u, v) !adjSet) && (u != v) then begin
            adjSet := Sadj.add (u, v) !adjSet;
            adjSet := Sadj.add (v, u) !adjSet;
            (*
            if (u != precolored) then begin

            end
            if (v != precolored) then begin

            end
        *)
        end
    ;;

    (*
    let main () =
        livenessAnaylsis ();
        build ();
        makeworklist ();
        let rec loop () =
            if simplifyWorklist != SS.empty then simplify ()
            else if worklistMoves != SS.empty then coalesce ()
            else if freezeWorklist != SS.empty then freeze ()
            else if spillWorklist != SS.empty then selectSpill ()
            ;
            if (simplifyWorklist != SS.empty || 
                worklistMoves != SS.empty ||
                freezeWorklist != SS.empty || 
                spillWorklist != SS.empty)
            then begin
                   loop ()
            end 

        in
        loop ()
    ;;
    *)

    let color (data : data) : allocation * Temp.temp list =
        (Temp.ITable.empty (), [])
    ;;
end
