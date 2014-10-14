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

let numColors = 4

type color = int

module SC = Set.Make(struct type t = int let compare = compare end)

let color (igraph : Liveness.igraph) =
    let open Liveness in
    print_endline "~~~~ ORIG GRAPH ~~~~";
    LG.show igraph.graph;
    print_endline "~~~~ END ORIG GRAPH ~~~~";
    let degree : int LGI.table = LGI.empty () in
    let removedStack : LG.node Stack.t = Stack.create () in
    (*let nodes = Graph.nodes igraph.graph in*)
    let nodeColors : color LGI.table = LGI.empty () in
    let newGraph = LG.newGraph() in (* Stores the rebuilt and colored graph *)
    let oldToNew : LG.node LGI.table = LGI.empty () in (* Maps orig_graph nodes to newGraph nodes *)
    let origSuccPreds : (LG.node list * LG.node list) LGI.table = LGI.empty () in
    let allColors = 
        let set = ref SC.empty in
        for i=0 to numColors-1 do set := SC.add i !set done;
        !set
    in
    (* Map old nodes to new nodes *)
    List.iter (fun oldNode ->
        (*let newNode = LG.newNode newGraph in*)
        (*LGI.enter(oldToNew, oldNode, newNode);*)
        LGI.enter(origSuccPreds, oldNode, (LG.succ oldNode, LG.pred oldNode))
    ) (LG.nodes igraph.graph);

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
            print_string "MIN DEG: "; LG.show_node n;
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
        print_string "ADJ TO: "; LG.show_node node;
        List.iter (fun n -> LG.show_node n) adj;
        let validColors = 
            List.fold_left (fun set n ->
                (* Check for cycles *)
                if not (LG.eq n node) then
                    SC.remove (LGI.look_exn nodeColors n) set
                else 
                    set
            ) allColors adj 
        in
        if SC.is_empty validColors then None
        else Some (SC.choose validColors)
    in

    (* Start popping nodes and coloring them *)
    let rec colorNodes () =
        print_endline "Coloring!";
        while not (Stack.is_empty removedStack) do
            let oldNode = Stack.pop removedStack in
            let newNode = LG.newNode newGraph in
            LGI.enter(oldToNew, oldNode, newNode);
            print_endline "POPPING NODE";
            LG.show_node oldNode;
            LG.show_node newNode;
            (* Fixup edges in new and old graphs *)
            (* XXX - If something goes wrong maybe it's due to multiple edges? *)
            let (succ, pred) = LGI.look_exn origSuccPreds oldNode in
            let nodes = LG.nodes newGraph in
            print_endline "~!@~ NEW GRAPH ~@!~";
            LG.show newGraph;
            print_endline "~!@~ END NEW GRAPH ~@!~";
            let nodeExists n = List.fold_left (fun res node -> if res then res else LG.eq n node) false nodes in
            List.iter (fun succ ->
                if nodeExists succ then (
                    let n = LGI.look_exn oldToNew succ in
                    LG.mk_edge {LG.from=newNode; to_=n}; print_endline "FOUND SUCC";
                )
            ) succ;
            List.iter (fun pred ->
                if nodeExists pred then (
                    let n = LGI.look_exn oldToNew pred in
                    LG.mk_edge {LG.from=n; to_=newNode}; print_endline "FOUND PRED";
                )
            ) pred;
            (* Pick node color *)
            match pickColor newNode with
            | Some c -> LGI.enter(nodeColors, newNode, c)
            | None -> (* Optimistic coloring failed, must do actual spill *)
                (* Rewrite program to put loads/stores around node *)
                (* Remake the liveness graph *)
                (* Redo coloring *)
                raise (Failure "Optimistic Spill Failure")
        done;
        print_endline "DONE COLORING!";
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
                    print_string "Removing node "; LG.show_node node;
                    print_endline (string_of_int (List.length (LG.nodes igraph.graph)));
                    Stack.push node removedStack;
                    LG.rm_node node;
                    print_string "Node removed? ";
                    print_endline (string_of_int (List.length (LG.nodes igraph.graph)));
                    updateDegrees();
                    simplify();
            | None -> raise (Failure "Spill Encountered") (* Spill *)
        end
    in
    simplify();
    (newGraph, nodeColors)
;;
