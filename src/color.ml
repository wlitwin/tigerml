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
    module FGI = Flowgraph.FGraph.ITable
    module FG = Flowgraph.FGraph
    module LG = Liveness.Graph
    module TI = Temp.ITable

    type move = LG.node * LG.node
    type color = int

    let moveList : Temp.temp LGI.table = LGI.empty ()

    module Sadj = Set.Make(struct type t = LG.node * LG.node let compare = compare end)
    module SN   = Set.Make(struct type t = LG.node           let compare = compare end)
    (*module SF   = Set.Make(struct type t = FG.node           let compare = compare end)*)
    module SG   = Set.Make(struct type t = Graph.node        let compare = compare end)
    module ST   = Set.Make(struct type t = Temp.temp         let compare = compare end)
    module SI   = Set.Make(struct type t = int               let compare = compare end)
    module SMov = Set.Make(struct type t = move              let compare = compare end)

    let simplifyWorklist : LG.node list ref = ref []
    let freezeWorklist   : LG.node list ref = ref []
    let spillWorklist    : SN.t ref = ref SN.empty
    let coalescedNodes   : LG.node list ref = ref []
    let coloredNodes     : LG.node list ref = ref []
    let selectStack      : LG.node Stack.t = Stack.create ()

    let worklistMoves = ref SG.empty
    (* coalescedMoves
     * constrainedMoves
     * frozenMoves
     * worklistMoves
     * activeMoves
     *)

    (* TODO - fix *)
    let precolored = SN.empty

    let adjSet   : Sadj.t ref = ref Sadj.empty
    let adjList  : SN.t LGI.table = LGI.empty ()
    let degree   : int LGI.table = LGI.empty ()
    let moveList : ST.t LGI.table = LGI.empty ()
    let alias : LG.node LGI.table = LGI.empty ()
    let color : color LGI.table = LGI.empty ()

    (* Number of registers from frame modules *)
    let numColors = 4

    let some = function
        | Some s -> s
        | None -> raise (Failure "Value was None")
    
    let get (func : ('a * 'b) -> 'c option) (table : 'a) (value : 'b) : 'c = 
        some (func (table, value))
    ;;

    let getTI  = get TI.look
    let getFGI = get FGI.look
    let getLGI (tbl : 'a LGI.table) (idx : 'b) : 'a = get LGI.look tbl idx
    let degreeOf node = getLGI degree node

    let addEdge (u, v : LG.node * LG.node) =
        if not (Sadj.mem (u, v) !adjSet) && (u != v) then begin
            adjSet := Sadj.add (u, v) !adjSet;
            adjSet := Sadj.add (v, u) !adjSet;
            if not (SN.mem u precolored) then (
                let adjListU : SN.t = getLGI adjList u in
                LGI.enter (adjList, u, SN.add v adjListU);
                LGI.enter (degree, u, (degreeOf u) + 1)
            );
            if not (SN.mem v precolored) then (
                let adjListV : SN.t = getLGI adjList v in
                LGI.enter (adjList, v, SN.add u adjListV);
                LGI.enter (degree, v, (degreeOf v) + 1)
            )
        end
    ;;

    type build_param = Liveness.igraph * (Flowgraph.FGraph.node -> Temp.temp list) * Flowgraph.flowgraph
    let build (igraph, liveOut, fgraph : build_param) =
        let open Liveness in
        let open Flowgraph in
        let blocks = LG.nodes igraph.graph in
        (* forall blocks in program *)
        List.iter (fun block ->
            let live = ref (ST.of_list (liveOut block)) in
            (* forall instructions(block) in reverse order *)
            let useSet = ST.of_list (getFGI fgraph.use block)
            and defSet = ST.of_list (getFGI fgraph.def block) in
            if some (FGI.look (fgraph.ismove, block)) then (
                live := ST.diff !live useSet;
                (* forall n in def(i) U use(i) *)
                let nDefUse = ST.union defSet useSet in
                ST.iter (fun n_temp ->
                    let n = igraph.tnode n_temp in
                    let moveListN : ST.t = getLGI moveList n in
                    LGI.enter (moveList, n, ST.add n_temp moveListN)
                ) nDefUse;
                worklistMoves := SG.add block !worklistMoves
            );
            live := ST.union !live defSet;
            ST.iter (fun d_temp ->
                let d = igraph.tnode d_temp in
                ST.iter (fun l_temp ->
                    let l = igraph.tnode l_temp in
                    addEdge (l, d)
                ) !live;
            ) defSet;
            live := ST.union useSet (ST.diff !live defSet);
        ) blocks;
    ;;

    let nodeMoves n =
        ST.diff (getTI moveList n) (ST.union !activeMoves !worklistMoves)
    ;;

    let moveRelated n =
        not (SN.is_empty (nodeMoves n))
    ;; 

    let makeWorklist initial =
        let copy = !initial in
        SN.iter (fun n ->
            initial := SN.remove n !initial; 
            if (getLGI degree n) >= numColors then
                spillWorklist := SN.add n !spillWorklist
            else if moveRelated n then
                freezeWorklist := SN.add n !freezeWorklist
            else 
                simplifyWorklist := SN.add n !simplifyWorklist
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
        set1 := SN.remove elt !set1;
        set2 := SN.add elt !set2
    ;;

    let adjacent n =
        SN.diff (getTI adjList n) (SN.union (stackToSet selectStack) !coalescedNodes)
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
        let d = degreeOf m in
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
        let (x, y) = SN.choose !worklistMoves in
        let x = getAlias x
        and y = getAlias y in
        let (u, v) =
            if SN.mem precolored y then (y, x)
            else (x, y)
        in
        worklistMoves := SN.remove !worklistMoves m;
        if (u = v) then (
            coalescedMoves := SN.add !coalescedMoves m
            addWorkList u
        ) else if SN.mem precolored v || SN.mem adjSet (u, v) then (
            constrainedMoves := SN.add !constrainedMoves m;
            addWorkList u;
            addWorkList v
        ) else if (SN.mem precolored u) && 
                  (SN.forall (fun t -> ok (t, u)) (adjacent v)) &&
                  (not SN.mem precolored u) && 
                  (conservative (SN.union (adjacent u) (adjacentv))) 
        then (
            coalescedMoves := SN.add !coalescedMoves m;
            combine (u, v);
            addWorkList u
        ) else (
            activeMoves := SN.add !activeMoves m
        )
    ;;

    let addWorkList u =
        if not SN.mem precolored u && 
           not moveRelated u && 
           degreeOf u < numColors 
        then
           moveEltFromTo freezeWorklist simplifyWorklist u
    ;;

    let ok (t, y) =
        (degreeOf t) < numColors || SN.mem precolored t || SN.mem adjSet (t, r)
    ;;

    let conservative nodes =
        let k = ref 0 in
        SN.iter (fun n ->
            if degreeOf n >= numColors then
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
        if (degreeOf u) >= numColors && SN.mem !freezeWorklist u then
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
            if SN.is_empty (nodeMoves v) && (degreeOf v) < numColors then (
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
                let colorUprecolored = SN.union !coloredNodes precolored in
                List.fold_left (fun set w ->
                    let aliasW = getAlias w in
                    if SN.mem colorUprecolored aliasW then
                        SN.remove set (getLGI color aliasW)
                    else 
                        set
                ) !okColors (get adjList n)
            in
            if SN.is_empty okColors then
                spilledNodes := SN.add !spilledNodes n
            else (
                coloredNodes := SN.add !coloredNodes n;
                let c = SN.choose okColors in
                LGI.enter (color, n, c)
            )
        done;
        SN.iter (fun n -> 
            let aliasN = getLGI color n in
            LGI.enter (color, n, aliasN)
        ) !coalescedNodes
    ;;

    let rewriteProgram () =
        (* Allocate memory locations for each v in spilledNodes *)
        (* Create a new temporary vi for each definition and each use *)
        (* In the porgram (instructions), insert a store after each definition of a
         * vi, a fetch before each use of vi. *)
        (* Put all the vi into a set newTemps *)
        (*
        spilledNodes <- {}
        initial <- coloredNodes U coalescedNodes U newTemps
        coloredNodes <- {}
        coalescedNodes <- {}
        *)
        (* Can be made faster by not throwing away all coalescedNodes.
         * Keep all the coalesces found before the first call to SelectSpill *)
        ()
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
