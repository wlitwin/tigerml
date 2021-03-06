module FG = Flowgraph.FGraph
module FGI = Flowgraph.FGraph.ITable
module TI = Temp.ITable
module Graph = Graph.Graph

let d_print_endline _ = ()
let d_print_string _ = ()

type igraph = { graph: Graph.graph; (* Interference graph *)
                tnode: Graph.node TI.table; (* Temps to the node *)
                gtemp: Temp.temp FGI.table; (* Node to the temps *)
                moves: (Graph.node * Graph.node) list } (* List of move instructions, hint to register allocator, 
                                                                       if (n, m) is on the list try to assign them to the same regs *)

(* If ismove for a node is true and def + use are the same, then it can be deleted *)

(* 1.) At any non-move instruction that defines a variable 'a', where the live-out variables are
 *     b1, ..., bj, add interference edges (a, b1) ... (a, bj)
 *
 * 2.) At a move instruction a <- c, where variables b1, ..., bj are live-out, add interference
 *     edges (a, b1) ... (a, bj) for any bi that is not the same as c
 *)

module SN = Set.Make(struct type t = FG.node let compare = compare end)
module ST = Set.Make(struct type t = Temp.temp let compare = compare end)

type liveSet = ST.t (*unit Temp.ITable.table * Temp.temp list*)
type liveMap = liveSet FG.ITable.table

let interferenceGraph (fgraph : Flowgraph.flowgraph) : igraph * ST.t FG.ITable.table =
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
    (*
    d_print_endline "TABLES MADE";
    FGI.iter (fun k v ->
        FG.show_node k
    ) fgraph.use;
    *)
    let rec computeLiveMap () =
        let allEqual = ref true in
        List.iter (fun n ->
            let inSet = FGI.look_exn liveInSet n
            and outSet = FGI.look_exn liveOutSet n in
            let in' = inSet
            and out' = outSet in
            let useSet = ST.of_list (FGI.look_exn fgraph.use n) in
            let defSet = ST.of_list (FGI.look_exn fgraph.def n) in
            let inSet = ST.union useSet (ST.diff outSet defSet) in
            (* Update the in[] map, so we can use it below *)
            FGI.enter (liveInSet, n, inSet);       
            let succ = FG.succ n in
            (*let outSet = List.fold_left (fun out s ->
                    ST.union out (FGI.look_exn liveInSet s)
                ) outSet succ
            in
            *)
            let outSet = List.fold_left (fun succIn s ->
                    ST.union succIn (FGI.look_exn liveInSet s)
                ) ST.empty succ
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
    d_print_endline "COMPUTING LIVEMAP";
    computeLiveMap ();
    (*
    d_print_endline "~~~~ LIVE IN ~~~~";
    FGI.iter (fun k v ->
        FG.show_node k; 
        print_string "  ";
        ST.iter (fun t ->
            print_string ((Temp.makestring t) ^ " ")
        ) v;
        print_endline "";
    ) liveInSet;
    print_endline "~~~~ LIVE OUT ~~~~";
    FGI.iter (fun k v ->
        FG.show_node k; 
        print_string "  ";
        ST.iter (fun t ->
            print_string ((Temp.makestring t) ^ " ")
        ) v;
        print_endline "";
    ) liveOutSet;
    print_endline "DONE";
    *)
    (*let liveMap : ST.t FGI.table = FGI.union_exn liveInSet liveOutSet in*)
    let liveMap : ST.t FGI.table = 
        let liveMap = FGI.clone liveInSet in
        FGI.iter (fun k v ->
            match FGI.look (liveMap, k) with
            | Some v1 -> FGI.enter (liveMap, k, ST.union v v1) 
            | None -> ()
        ) liveOutSet;
        liveMap
    in
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
            (* a move instruction a <- c only put an edge 
             * from for all live out nodes that are not c.
             * a = def, c = use
             *)
            let def = ST.of_list (FGI.look_exn fgraph.def n) in
            let live = FGI.look_exn liveOutSet n in
            (*if FGI.look_exn fgraph.ismove n then (
                let use = ST.of_list (FGI.look_exn fgraph.use n) in
                if ST.cardinal def != 1 then failwith "Expected only one def";
                if ST.cardinal use != 1 then failwith "Expected only one use";
                let a : Temp.temp = ST.choose def in
                let c : Temp.temp = ST.choose use in
                print_endline ("Move " ^ (Frame_x86.string_of_temp a) ^ " <- " ^ (Frame_x86.string_of_temp c));
                let an = getNode a in
                ST.iter (fun bj ->
                    if not (bj = c) then (
                        let bn = getNode bj in
                        print_endline ("Edge: " ^ (Frame_x86.string_of_temp a) ^ " to " ^ (Frame_x86.string_of_temp bj));
                        Graph.mk_edge {from = an; to_ = bn};
                        Graph.mk_edge {from = bn; to_ = an}
                    )
                ) live;
            ) else (
                *)
                ST.iter (fun d ->
                    let dn = getNode d in
                    ST.iter (fun temp ->
                        let tn = getNode temp in
                        print_endline ("Edge NM: " ^ (Frame_x86.string_of_temp d) ^ " to " ^ (Frame_x86.string_of_temp temp));
                        Graph.mk_edge {from = dn; to_ = tn};
                        Graph.mk_edge {from = tn; to_ = dn}
                    ) live;
                ) def;
            (*)*)
        ) nodes;
        (g, tempToNode, nodeToTemp)
    in
    d_print_endline "CREATING IGRAPH";
    flush stdout;
    let (graph, tnode, gtemp) = createIGraph () in
    (* TODO populate the moves list *)
    ({ graph; tnode; gtemp; moves = [] }, liveOutSet)
;;

let show (chan, igraph : out_channel * igraph) : unit =
    let nodes = Graph.nodes igraph.graph in
    print_endline ("NODES: " ^ (string_of_int (List.length nodes)));
    List.iteri (fun i n -> (* For all nodes *)
        let temp = FGI.look_exn igraph.gtemp n in
        let str = Frame_x86.string_of_temp temp in
        Printf.fprintf chan "Node (%d) %s\n  Succ: " i str;
        let interSucc = Graph.succ n in 
        let interPred = Graph.pred n in
        List.iter (fun inode -> (* For all neighbors *)
            let istr = (*Temp.makestring*)Frame_x86.string_of_temp (FGI.look_exn igraph.gtemp inode) in
            Printf.fprintf chan "%s " istr;
        ) interSucc;
        print_string "\n  Pred: ";
        List.iter (fun inode -> (* For all neighbors *)
            let istr = (*Temp.makestring*)Frame_x86.string_of_temp (FGI.look_exn igraph.gtemp inode) in
            Printf.fprintf chan "%s " istr;
        ) interPred;
        Printf.fprintf chan "\n";
    ) nodes
;;
