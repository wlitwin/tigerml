module FG = Flowgraph.FGraph
module FGI = Flowgraph.FGraph.ITable
module TI = Temp.ITable
module Graph = Graph.Graph

type igraph = { graph: Graph.graph; (* Interference graph *)
                tnode: Temp.temp -> Graph.node; (* Temps to the node *)
                gtemp: Graph.node -> Temp.temp; (* Node to the temps *)
                moves: (Graph.node * Graph.node) list } (* List of move instructions, hint to register allocator, 
                                                                       if (n, m) is on the list try to assign them to the same regs *)

type liveSet = unit Temp.ITable.table * Temp.temp list
type liveMap = liveSet FG.ITable.table

(* If ismove for a node is true and def + use are the same, then it can be deleted *)

(* 1.) At any non-move instruction that defines a variable 'a', where the live-out variables are
 *     b1, ..., bj, add interference edges (a, b1) ... (a, bj)
 *
 * 2.) At a move instruction a <- c, where variables b1, ..., bj are live-out, add interference
 *     edges (a, b1) ... (a, bj) for any bi that is not the same as c
 *)

module SN = Set.Make(struct type t = FG.node let compare = compare end)
module ST = Set.Make(struct type t = Temp.temp let compare = compare end)

let interferenceGraph (fgraph : Flowgraph.flowgraph) : igraph * (Flowgraph.FGraph.node -> Temp.temp list) =
    (* Out function maps nodes to live-out temporaries *)  
    (* Initialize sets *)
    let open FG in
    let open Flowgraph in
    let nodes = List.rev (FG.nodes fgraph.control) in

    (* For each n in[n] <- {}; out[n] <- {} *)
    let liveInSet : ST.t FG.ITable.table = FG.ITable.empty () in
    let liveOutSet : ST.t FG.ITable.table = FG.ITable.empty () in
    List.iter (fun n ->
        FG.ITable.enter (liveInSet,  n, ST.empty);
        FG.ITable.enter (liveOutSet, n, ST.empty);
    ) nodes;
    (* repeat
     *   for each n
     *     in'[n] <- in[n]; out'[n] <- out[n]
     *     in[n] <- use[n] U (out[n] - def[n])
     *     out[n] U(s in succ[n]) in[s]
     *   until in'[n] = in[n] and out'[n] = out[n] for all n
     *)
    let some opt = match opt with Some s -> s | None -> raise ErrorMsg.Error in
    let rec computeLiveMap () =
        let allEqual = ref true in
        List.iter (fun n ->
            let inSet = some (FGI.look (liveInSet, n))
            and outSet = some (FGI.look (liveOutSet, n)) in
            let in' = inSet
            and out' = outSet in
            let useSet = ST.of_list (some (FGI.look (fgraph.use, n))) in
            let defSet = ST.of_list (some (FGI.look (fgraph.def, n))) in
            let inSet = ST.union useSet (ST.diff outSet defSet) in
            (* Update the in[] map, so we can use it below *)
            FGI.enter (liveInSet, n, inSet);       
            let succ = FG.succ n in
            let outSet = List.fold_left (fun out s ->
                    ST.union out (some (FGI.look (liveInSet, s)))
                ) outSet succ
            in
            (* Update the out[] map *)
            FGI.enter (liveOutSet, n, outSet);
            (* Check if in' == in and out' == out *)
            if not (ST.equal in' inSet) || not (ST.equal out' outSet) then (
                allEqual := false
            );
        ) nodes;
        if not !allEqual then (
            computeLiveMap ()
        )
    in
    (* Compute liveness *)
    computeLiveMap ();
    let liveMap : ST.t FGI.table = FGI.union_exn liveInSet liveOutSet in
    (* liveInSet and liveOutSet now have the liveIn + liveOut sets for all nodes *)
    let createIGraph () =
        let g = Graph.newGraph () in
        let tempToNode : Graph.node TI.table = TI.empty () in
        let nodeToTemp : Temp.temp FGI.table = FGI.empty () in
        let getNode temp =
            match TI.look (tempToNode, temp) with
            | Some n -> n
            | None -> let newNode = Graph.newNode g in
                      TI.enter (tempToNode, temp, newNode);
                      FGI.enter (nodeToTemp, newNode, temp);
                      newNode
        in
        List.iter (fun n ->
            (* TODO - check if it's a move and don't add interference for specific temporary *)
            let def = ST.of_list (some (FGI.look (fgraph.def, n))) in
            let live = some (FGI.look (liveMap, n)) in
            ST.iter (fun d ->
                let dn = getNode d in
                ST.iter (fun temp ->
                    let tn = getNode temp in
                    Graph.mk_edge {from = dn; to_ = tn}
                ) live;
            ) def;
        ) nodes;
        (g, tempToNode, nodeToTemp)
    in
    let (graph, tnode, gtemp) = createIGraph () in
    let tnode = fun temp -> some (TI.look (tnode, temp)) in
    let gtemp = fun node -> some (FGI.look (gtemp, node)) in
    let list_of_set set = ST.fold (fun elt lst -> elt :: lst) set [] in
    (* TODO populate the moves list *)
    ({ graph; tnode; gtemp; moves = [] }, fun node -> list_of_set (some (FGI.look (liveOutSet, node))))
;;

let show (chan, igraph : out_channel * igraph) : unit =
    let nodes = Graph.nodes igraph.graph in
    List.iter (fun n ->
        let temp = igraph.gtemp n in
        let str = Temp.makestring temp in
        Printf.fprintf chan "%s - " str;
        let interferenceNodes = Graph.adj n in 
        List.iter (fun inode ->
            let istr = Temp.makestring (igraph.gtemp inode) in
            Printf.fprintf chan "%s " istr;
        ) interferenceNodes;
        Printf.fprintf chan "\n";
    ) nodes
;;
