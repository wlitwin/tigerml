(* let k = number of machine registers
 *
 * NP - Hard problem
 *
 * RIG - Register interference graph
 *
 * Pick node with fewer than k neighbors in RIG
 *
 * Eliminate t and its edges from RIG
 *
 * If the resulting graph has a k-coloring then so does the original graph
 *
 * 1 -- Pick node t with fewer than k neighbors
 * 2 -- Put t on a stack and remove it from the RIG
 * 3 -- Repeat until the graph has one node
 *
 * 4 -- Start assigning colors to nodes on the stack
 *    |-- Pick a color different than one assigned to all neighbors
 * 
 * 5 -- If heuristic fails:
 *    |-- Pick a candidate for spilling
 *    |-- Remove candidate and continue the simplification
 *    |-- During assignment, hope that it's colorable
 *      |-- If fails to color, then we must actually spill f
 *      |-- Rewrite program to load/store f
 *        |-- Before each operation that uses f, insert f := load fa
 *        |-- After each operation that defines f, insert store f, fa
 *      |-- Recreate liveness graph
 *
 * Possible spill heuristics:
 *    - Spill temporaries with most conflicts
 *    - Spill temporaries with few definitions and uses
 *    - Avoid spilling in inner loops
 *)

module Graph = Graph.Graph
module LG = Liveness.Graph
module LGI = LG.ITable
module TI = Temp.ITable

let d_print_endline _ = ()
let d_print_string _ = ()

type color = int

module SC = Set.Make(struct type t = int let compare = compare end)

type precolored = unit TI.table

let color (igraph : Liveness.igraph) (precoloring : precolored) (numColors : int) : (LG.graph * int TI.table) =
    let open Liveness in
    d_print_endline "~~~~ ORIG GRAPH ~~~~";
    (*LG.show igraph.graph;*)
    let orig_graph = LG.copy igraph.graph in
    d_print_endline "~~~~ END ORIG GRAPH ~~~~";
    let degree : int LGI.table = LGI.empty () in
    let removedStack : LG.node Stack.t = Stack.create () in
    (*let nodes = Graph.nodes igraph.graph in*)
    let nodeColors : color LGI.table = LGI.empty () in
    (*
    let newGraph = LG.newGraph() in (* Stores the rebuilt and colored graph *)
    let origSuccPreds : (LG.node list * LG.node list) LGI.table = LGI.empty () in
*)
    let oldToNew : LG.node LGI.table = LGI.empty () in (* Maps orig_graph nodes to newGraph nodes *)
    
    List.iter2 (fun nOld nNew ->
        if not (LG.eq nOld nNew) then failwith "BAALAH";
        LGI.enter(oldToNew, nOld, nNew);
    ) (LG.nodes igraph.graph) (LG.nodes orig_graph);

    let allColors = 
        let set = ref SC.empty in
        for i=0 to numColors-1 do set := SC.add i !set done;
        !set
    in
    (*
    (* Map old nodes to new nodes *)
    List.iter (fun oldNode ->
        let newNode = LG.newNode newGraph in
        LGI.enter(oldToNew, oldNode, newNode);
        LGI.enter(origSuccPreds, oldNode, (LG.succ oldNode, LG.pred oldNode))
    ) (LG.nodes igraph.graph);
    *)

    (* Set the current degree for all nodes *)
    let updateDegrees() =
        List.iter (fun n ->
            let numNeighbors = List.length (Graph.adj n) in
            LGI.enter(degree, n, numNeighbors)
        ) (LG.nodes igraph.graph);
    in
    updateDegrees();
    (* Node stack *)

    let pickMinDegree () =
        let nodes = LG.nodes igraph.graph in
        let (node, minDeg) = (ref (List.hd nodes), ref 0) in
        let found = ref false in
        List.iter (fun n ->
            (*d_print_string "MIN DEG: "; LG.show_node n;*)
            let d = LGI.look_exn degree n in
            if (not !found || d < !minDeg) then begin
                node := n; minDeg := d; found := true;
            end
        ) nodes;
        if !found then Some (!node, !minDeg) else None
    in

    let pickColor node =
        (* All previously added nodes should already be colored *)
        let adj = LG.adj node in
        (*d_print_string "ADJ TO: "; LG.show_node node;*)
        (*List.iter (fun n -> LG.show_node n) adj;*)
        let validColors = 
            List.fold_left (fun set n ->
                match LGI.look(nodeColors, n) with
                | Some c -> SC.remove c set
                | None -> set
            ) allColors adj 
        in
        d_print_string "Available colors: ";
        SC.iter (fun c -> d_print_string ((string_of_int c) ^ " ")) validColors;
        if SC.is_empty validColors then (
            d_print_endline "No more valid colors";
            None
        ) else ( 
            let temp = LGI.look_exn igraph.gtemp node in
            match TI.look(precoloring, temp) with
            | Some _ ->
                    d_print_endline "Temp is precolored";
                    if not (SC.mem temp validColors) then (
                        d_print_endline "Color not available";
                        None
                    ) else (
                        d_print_endline "Can use precoloring";
                        Some temp
                    )
            | None -> 
                Some (SC.choose validColors)
        )
    in

    (* Start popping nodes and coloring them *)
    let rec colorNodes () =
        d_print_endline "Coloring!";
        while not (Stack.is_empty removedStack) do
            let node = LGI.look_exn oldToNew (Stack.pop removedStack) in
            (* Pick node color *)
            match pickColor node with
            | Some c -> LGI.enter(nodeColors, node, c)
            | None -> (* Optimistic coloring failed, must do actual spill *)
                (* Rewrite program to put loads/stores around node *)
                (* Remake the liveness graph *)
                (* Redo coloring *)
                raise (Failure "Optimistic Spill Failure")
                (*LGI.enter(nodeColors, newNode, 0)*)
        done;
        d_print_endline "DONE COLORING!";
    (* Step 1 remove nodes until there is only one node in the graph *)
    and simplify () =
        if (LG.numNodes igraph.graph = 0) then
            (* Step 3 - all nodes have been removed from the graph *)
            colorNodes()
        else begin
            (* Pick node of degree < k *)
            match pickMinDegree() with
            | Some (node, minDeg) ->
                    (* Step 2 remove node from the graph *)
                    (*d_print_string "Removing node "; LG.show_node node;*)
                    d_print_endline (string_of_int (List.length (LG.nodes igraph.graph)));
                    Stack.push node removedStack;
                    LG.rm_node node;
                    d_print_string "Node removed? ";
                    d_print_endline (string_of_int (List.length (LG.nodes igraph.graph)));
                    updateDegrees();
                    simplify();
            | None -> raise (Failure "Spill Encountered") (* Spill *)
        end
    in
    simplify();
    let tempColorings : color TI.table =
        let tbl = TI.empty () in
        let nodes = LG.nodes orig_graph in
        List.iter (fun n ->
            TI.enter(tbl, LGI.look_exn igraph.gtemp n, LGI.look_exn nodeColors n)
        ) nodes;
        tbl
    in
    (orig_graph, tempColorings)
;;
