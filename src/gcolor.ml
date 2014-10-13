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
    let degree : int LGI.table = LGI.empty () in
    let removedStack : LG.node Stack.t = Stack.create () in
    let nodes = Graph.nodes igraph.graph in
    let nodeColors : color LGI.table = LGI.empty () in
    let newGraph = LG.newGraph() in (* Stores the rebuilt and colored graph *)
    let oldToNew : LG.node LGI.table = LGI.empty () in (* Maps orig_graph nodes to newGraph nodes *)
    let allColors = 
        let set = ref SC.empty in
        for i=0 to numColors-1 do set := SC.add i !set done;
        !set
    in
    (* Map old nodes to new nodes *)
    List.iter (fun oldNode ->
        let newNode = LG.newNode newGraph in
        LGI.enter(oldToNew, oldNode, newNode)
    ) nodes;

    (* Set the current degree for all nodes *)
    let updateDegrees() =
        List.iter (fun n ->
            let numNeighbors = List.length (Graph.adj n) in
            LGI.enter(degree, n, numNeighbors)
        ) nodes;
    in
    updateDegrees();
    (* Node stack *)

    let pickMinDegree () =
        let (node, minDeg) = (ref (List.hd nodes), ref 0) in
        let found = ref false in
        LGI.iter (fun n d ->
            if (not !found || d < !minDeg) then begin
                node := n; minDeg := d; found := true;
            end
        ) degree;
        if !found then Some (!node, !minDeg) else None
    in

    let pickColor node =
        (* All previously added nodes should already be colored *)
        let adj = LG.adj node in
        let validColors = 
            List.fold_left (fun set n ->
                SC.remove (LGI.look_exn nodeColors n) set
            ) allColors adj 
        in
        if SC.is_empty validColors then None
        else Some (SC.choose validColors)
    in

    (* Start popping nodes and coloring them *)
    let rec colorNodes () =
        while not (Stack.is_empty removedStack) do
            let oldNode = Stack.pop removedStack in
            let newNode = LGI.look_exn oldToNew oldNode in
            (* Fixup edges in new and old graphs *)
            (* XXX - If something goes wrong maybe it's due to multiple edges? *)
            let succ = LG.succ oldNode in
            let pred = LG.pred oldNode in
            List.iter (fun succ ->
                match LGI.look(oldToNew, succ) with
                | Some n -> LG.mk_edge {LG.from=newNode; to_=n}
                | None -> ()
            ) succ;
            List.iter (fun pred ->
                match LGI.look(oldToNew, pred) with
                | Some n -> LG.mk_edge {LG.from=n; to_=newNode}
                | None -> ()
            ) pred;
            (* Pick node color *)
            match pickColor newNode with
            | Some c -> LGI.enter(nodeColors, newNode, c)
            | None -> (* Optimistic coloring failed, must do actual spill *)
                (* Rewrite program to put loads/stores around node *)
                (* Remake the liveness graph *)
                (* Redo coloring *)
                raise (Failure "Optimistic Spill Failure")
        done
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
                    Stack.push node removedStack;
                    LG.rm_node node;
                    updateDegrees();
                    simplify();
            | None -> raise (Failure "Spill Encountered") (* Spill *)
        end
    in
    simplify();
    (newGraph, nodeColors)
;;
