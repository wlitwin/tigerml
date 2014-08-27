module T = Tree

let linearize stm =
    let rec commute = function
        | (T.EXP (T.CONST _), _) -> true
        | (_, T.NAME _) -> true
        | (_, T.CONST _) -> true
        | _ -> false
    in

    let nop = T.EXP (T.CONST 0) in

    let ( % ) x y = match (x, y) with
        | (T.EXP (T.CONST _), x) -> x
        | (x, T.EXP (T.CONST _)) -> x
        | (x, y) -> T.SEQ (x, y)
    in

    let rec reorder = function
        | (T.CALL _ as e) :: rest ->
                let t = Temp.newtemp () in
                reorder ((T.ESEQ (T.MOVE (T.TEMP t, e), T.TEMP t)) :: rest)
        | a :: rest ->
                let (stms, e) = do_exp a in
                let (stms', el) = reorder rest in
                if commute (stms', e) then
                    (stms % stms', e :: el)
                else
                    let t = Temp.newtemp () in
                    (stms % T.MOVE (T.TEMP t, e) % stms', (T.TEMP t) :: el)
        | [] -> (nop, [])

    and reorder_exp (el, build) =
        let (stms, el') = reorder el in
        (stms, build el')

    and reorder_stm (el, build) =
        let (stms, el') = reorder (el) in
        stms % build (el')

    and do_stm = function
        | T.SEQ (a, b) -> do_stm a % do_stm b
        | T.JUMP (e, labs) ->
                let build_exp = function
                    | [e] -> T.JUMP (e, labs) | _ -> assert false
                in
                reorder_stm ([e], build_exp)
        | T.CJUMP (p, a, b, t, f) ->
                let build_exp = function
                    | [a; b] -> T.CJUMP (p, a, b, t, f) | _ -> assert false
                in
                reorder_stm ([a; b], build_exp)
        | T.MOVE (T.TEMP t, T.CALL (e, el)) ->
                let build_exp = function
                    | (e :: el) -> T.MOVE (T.TEMP t, T.CALL (e, el)) | _ -> assert false
                in
                reorder_stm (e :: el, build_exp)
        | T.MOVE (T.TEMP t, b) ->
                let build_exp = function
                    | [b] -> T.MOVE (T.TEMP t, b) | _ -> assert false
                in
                reorder_stm ([b], build_exp)
        | T.MOVE (T.MEM e, b) ->
                let build_exp = function
                    | [e; b] -> T.MOVE (T.MEM e, b) | _ -> assert false
                in
                reorder_stm ([e; b], build_exp)
        | T.MOVE (T.ESEQ (s, e), b) ->
                do_stm (T.SEQ (s, T.MOVE (e, b)))
        | T.EXP (T.CALL (e, el)) ->
                let build_exp = function
                    | (e :: el) -> T.EXP (T.CALL (e, el)) | _ -> assert false
                in
                reorder_stm (e :: el, build_exp)
        | T.EXP e ->
                let build_exp = function
                    | [e] -> T.EXP e | _ -> assert false
                in
                reorder_stm ([e], build_exp)
        | s -> 
                let build_exp = function
                    | [] -> s | _ -> assert false
                in
                reorder_stm ([], build_exp)
    and do_exp = function
        | T.BINOP (p, a, b) ->
                let build_exp = function
                    | [a; b] -> T.BINOP (p, a, b) | _ -> assert false
                in
                reorder_exp ([a; b], build_exp)
        | T.MEM a ->
                let build_exp = function
                    | [a] -> T.MEM a | _ -> assert false
                in
                reorder_exp ([a], build_exp)
        | T.ESEQ (s, e) ->
                let stms = do_stm s in
                let (stms', e) = do_exp e in
                (stms % stms', e)
        | T.CALL (e, el) ->
                let build_exp = function
                    | (e :: el) -> T.CALL (e, el) | _ -> assert false
                in
                reorder_exp (e :: el, build_exp)
        | e -> 
                let build_exp = function
                    | [] -> e | _ -> assert false
                in
                reorder_exp ([], build_exp)
    in

    let rec linear = function
        | (T.SEQ (a, b), l) -> linear (a, linear (b, l))
        | (s, l) -> s :: l
    in

    linear (do_stm stm, [])
;;

type block = T.stm list

let basicBlocks stms =
    let _done = Temp.newlabel () in
    let rec blocks = function
        | ((T.LABEL _ as head) :: tail, blist) ->
                let rec next = function
                    | ((T.JUMP _ as s) :: rest, thisblock) ->
                            endblock (rest, s :: thisblock)
                    | ((T.CJUMP _ as s) :: rest, thisblock) ->
                            endblock (rest, s :: thisblock)
                    | ((T.LABEL lab) :: _ as stms, thisblock) ->
                            next (T.JUMP (T.NAME lab, [lab]) :: stms, thisblock)
                    | (s :: rest, thisblock) ->
                            next (rest, s :: thisblock)
                    | ([], thisblock) ->
                            next ([T.JUMP (T.NAME _done, [_done])], thisblock)
                and endblock (stms, thisblock) =
                    blocks (stms, List.rev (thisblock :: blist))
                in
                next (tail, [head])
        | ([], blist) -> List.rev blist
        | (stms, blist) ->
                blocks (T.LABEL (Temp.newlabel ()) :: stms, blist)
    in
    (blocks (stms, []), _done)
;;

let enterblock : (T.stm list * T.stm list Symbol.table) -> T.stm list Symbol.table = function
    | ((T.LABEL s :: _ as b), table) -> Symbol.enter (table, s, b)
    | (_, table) -> table
;;

let rec splitlast : T.stm list -> T.stm list * T.stm = function
    | [] -> assert false
    | [x] -> ([], x)
    | h :: t -> 
            let (t', last) = splitlast t in
            (h :: t', last)
;;

let rec trace = function 
    | (table, ((T.LABEL lab) :: _ as b), rest) ->
            let table = Symbol.enter (table, lab, []) in
            (match splitlast b with
            | (most, T.JUMP (T.NAME lab, _)) ->
                    (match Symbol.look (table, lab) with
                    | Some (_ :: _ as b') -> most @ (trace (table, b', rest))
                    | _ -> b @ (getnext (table, rest))
                    )
            | (most, T.CJUMP (op, x, y, t, f)) ->
                    (match (Symbol.look (table, t), Symbol.look (table, f)) with
                    | (_, Some (_ :: _ as b')) -> b @ (trace (table, b', rest))
                    | (Some (_ :: _ as b'), _) -> 
                            most @ [T.CJUMP (T.notRel op, x, y, f, t)]
                                 @ (trace (table, b', rest))
                    | _ -> 
                            let f' = Temp.newlabel () in
                            most @ [T.CJUMP (op, x, y, t, f'); 
                                    T.LABEL f'; T.JUMP (T.NAME f, [f])]
                                 @ (getnext (table, rest))
                    )
            | (most, T.JUMP _) -> b @ (getnext (table, rest))
            | _ -> failwith "Internal compiler error"
            )
    | _ -> failwith "Internal compiler error"

and getnext = function
    | (table, (T.LABEL lab :: _ as b) :: rest) ->
            (match Symbol.look (table, lab) with
            | Some (_ :: _) -> trace (table, b, rest)
            | _ -> getnext (table, rest)
            )
    | (table, []) -> []
    | _ -> assert false
;;


let traceSchedule (blocks, exit_label : T.stm list list * T.label) =
    let res : T.stm list =
        let tbl : T.stm list Symbol.table = 
            List.fold_right 
                (fun acc tbl -> enterblock (acc, tbl)) 
                blocks
                (Symbol.empty ())
        in
        getnext (tbl, blocks)
    in
    res @ [T.LABEL exit_label]
;;
