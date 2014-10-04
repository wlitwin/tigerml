module A = Assem
module F = Flowgraph

let instrs2graph (instrs : Assem.instr list) : Flowgraph.flowgraph * Flowgraph.FGraph.node list =
    let open A in
    let module FG = F.FGraph in
    let g = FG.newGraph () in
    let labelNodes : (Temp.label, FG.node) Hashtbl.t = Hashtbl.create 10 in
    (* Add instructions one after another, ignoring jumps *)
    let add_instr_simple accum instr = 
        (* Reverses the node list *)
        let newNode = FG.newNode g in
        begin match instr with
        | A.LABEL { assem; lab } -> Hashtbl.add labelNodes lab newNode
        | _ -> ()
        end;
        (newNode, instr) :: accum
    in
    let add_jumps node jumpList = 
        List.iter (fun lab -> 
            let jnode = Hashtbl.find labelNodes lab in
            FG.mk_edge { FG.from = node; to_ = jnode }
        ) jumpList;
    in
    let add_edges (nextNode, nodeList, nodeInstrList : FG.node option * FG.node list * ((FG.node * A.instr) list)) 
                  (node, instr : FG.node * A.instr) =
        (* Nodes are in reverse order at this point *)
        begin match (nextNode, instr) with
        | (Some next, A.OPER { assem; dst = _; src = _; jump = Some jumpList }) ->
                add_jumps node jumpList;
                FG.mk_edge { FG.from = node; to_ = next }
        | (Some next, A.OPER { assem; dst = _; src = _; jump = None})
        | (Some next, A.MOVE { assem; dst = _; src = _ }) ->
                FG.mk_edge { FG.from = node; to_ = next }
        | (Some next, A.LABEL { assem; lab }) ->
                FG.mk_edge { FG.from = node; to_ = next }
        | (None, A.OPER { assem; dst = _; src = _; jump = Some jumpList }) -> add_jumps node jumpList
        | (None, A.OPER _) | (None, A.MOVE _) | (None, A.LABEL _) -> ()
        end;
        (Some node, node :: nodeList, (node, instr) :: nodeInstrList)
    in
    let use_tbl : Temp.temp list FG.ITable.table = FG.ITable.empty () in
    let def_tbl : Temp.temp list FG.ITable.table = FG.ITable.empty () in
    let move_tbl : bool FG.ITable.table = FG.ITable.empty () in
    let add_all_to_table tbl node lst = 
        List.iter (fun tmp -> 
            match FG.ITable.look (tbl, node) with
            | Some lst -> FG.ITable.enter (tbl, node, tmp :: lst)
            | None -> FG.ITable.enter (tbl, node, [tmp])
        ) 
        lst 
    in
    let create_tables : (FG.node * A.instr) -> unit = function
        | (node, A.OPER {assem; dst; src; jump}) ->
                add_all_to_table use_tbl node dst;
                add_all_to_table use_tbl node src;
                FG.ITable.enter (move_tbl, node, false)
        | (node, A.MOVE {assem; dst; src}) ->
                FG.ITable.enter (use_tbl, node, [dst; src]);
                FG.ITable.enter (def_tbl, node, [dst]);
                FG.ITable.enter (move_tbl, node, true)
        | (node, A.LABEL _) ->
                FG.ITable.enter (move_tbl, node, false)
    in
    (* Reverses node list *)
    let instrNodeList : (FG.node * A.instr) list = List.fold_left add_instr_simple [] instrs in
    (* Unreverses node list *)
    let (_, nodeList, nodeInstrList) = List.fold_left add_edges (None, [], []) instrNodeList in
    List.iter create_tables nodeInstrList;
    ({ F.control = g; def = def_tbl; use = use_tbl; ismove = move_tbl }, nodeList)
;;
