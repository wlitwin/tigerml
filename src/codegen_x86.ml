module A = Assem
module S = Symbol
module T = Tree

let codegen (frame : Frame_x86.frame) (stm : Tree.stm) : Assem.instr list =
    let ( $ ) a b c = b (a c)
    and ( $! ) a b = a b in
    let linear = Canon.linearize stm in
    print_endline "---- LINEAR ----";
    List.iter (fun t ->
        Print_tree.print t
    ) linear;
    print_endline "---- LINEAR END ----";
    (*
    print_endline "---- BASIC BLOCKS START ----";
    let (blocks, lab) = Canon.basicBlocks (Canon.linearize stm) in
    List.iteri (fun i lst ->
        print_endline ("block " ^ (string_of_int i));
        List.iter (fun i ->
            Print_tree.print i
        ) lst;
    ) blocks;
    print_endline "---- BASIC BLOCKS END ----";
    *)
    let stmlist = let open Canon in
        linearize $ basicBlocks $ traceSchedule $! stm
    in
    print_endline "==== FINAL STATEMENTS ====";
    List.iter (fun s ->
        Print_tree.print s
    ) stmlist;
    print_endline "==== END FINAL STATEMENTS ====";
    let ilist = ref ([] : A.instr list) in
    let emit x = ilist := x :: !ilist in
    let result gen : Temp.temp =
        let t = Temp.newtemp () in
        gen t; t
    in
    let itos = string_of_int in
    let open Assem in
    let rel_str = function 
        | T.EQ -> "jz"
        | T.NE -> "jnz"
        | T.LT -> "jl"
        | T.LE -> "jle"
        | T.GT -> "jg"
        | T.GE -> "jge"
        | T.ULT -> "jb"
        | T.ULE -> "jbe"
        | T.UGT -> "ja"
        | T.UGE -> "jae"
    in
    let rec munchStm : T.stm -> unit = function
        (* mov [addr], e2 *)
        (*| T.MOVE (T.MEM e1, e2) ->
                emit (A.MOVE {assem="mov [`d0], `s0";
                              dst=munchExp e1;
                              src=munchExp e2})
        | T.MOVE (e1, T.MEM e2) ->
                emit (A.MOVE {assem="mov `d0, [`s0]";
                              dst=munchExp e1;
                              src=munchExp e2})
                              *)
        (* Munch sources before destinations *)
        | T.CJUMP (op, T.CONST c1, T.CONST c2, tlab, flab) ->
                let temp = T.TEMP (Temp.newtemp()) in
                munchStm (T.MOVE (temp, T.CONST c1));
                munchStm (T.CJUMP (op, temp, T.CONST c2, tlab, flab))
        | T.CJUMP (op, T.TEMP e1, T.CONST c2, tlab, flab) ->
                emit (A.OPER {assem="cmp `s0, " ^ (itos c2); dst=[]; src=[e1]; jump=None});
                emit (A.OPER {assem=(rel_str op) ^ " " ^ (Symbol.name tlab); dst=[]; src=[]; jump=Some[tlab; flab]})
        | T.CJUMP (op, e1, e2, tlab, flab) ->
                emit (A.OPER {assem="cmp `s0, `s1"; dst=[]; src=[munchExp e1; munchExp e2]; jump=None});
                emit (A.OPER {assem=(rel_str op) ^ " " ^ (Symbol.name tlab); dst=[]; src=[]; jump=Some[tlab; flab]})
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c1)), T.CONST c2) ->
                emit (A.OPER {assem="mov dword [`s0+" ^ (itos c1) ^ "], " ^ (itos c2);
                              dst=[];
                              src=[munchExp e1]; jump=None})
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c1)), T.NAME l2) ->
                emit (A.OPER {assem="mov dword [`s0+" ^ (itos c1) ^ "], " ^ (Symbol.name l2);
                              dst=[];
                              src=[munchExp e1]; jump=None})
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP e1, T.CONST c1)), e2) ->
                emit (A.OPER {assem="mov [`s0+" ^ (itos c1) ^ "], `s1"; src=[e1; munchExp e2]; dst=[]; jump=None})
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c)), e2) ->
                emit (A.OPER {assem="mov [`s0+" ^ (itos c) ^ "], `s1";
                              src=[munchExp e1; munchExp e2];
                              dst=[]; jump=None})
        | T.MOVE (e1, (T.MEM (T.BINOP (T.PLUS, e2, T.CONST c)))) ->
                emit (A.MOVE {assem="mov `d0, [`s0+" ^ (itos c) ^ "]";
                              src=munchExp e2;
                              dst=munchExp e1})
        | T.MOVE (T.TEMP e1, T.BINOP (T.PLUS, T.TEMP e2, T.CONST c2)) when e1 = e2 ->
                emit (A.OPER {assem="add `d0, " ^ (itos c2); dst=[e1]; src=[e1]; jump=None})
        | T.MOVE (T.TEMP e1, T.BINOP (T.PLUS, T.TEMP e2, T.CONST c2)) ->
                emit (A.MOVE {assem="mov `d0, `s0"; dst=e1; src=e2;});
                emit (A.OPER {assem="add `d0, " ^ (itos c2); dst=[e1]; src=[e1]; jump=None})
        | T.MOVE (T.MEM e1, e2) ->
                emit (A.OPER {assem="mov [`s0], `s1"; src=[munchExp e1; munchExp e2]; dst=[]; jump=None})
        | T.MOVE (e1, T.CONST i) ->
                emit (A.OPER {assem="mov `d0, " ^ (itos i);
                              dst=[munchExp e1];
                              src=[]; jump=None})
        | T.MOVE (e1, e2) ->
                emit (A.MOVE {assem="mov `d0, `s0"; src=munchExp e2; dst=munchExp e1})
        | T.LABEL lab ->
                emit (A.LABEL {assem = (S.name lab) ^ ":"; lab})
        | T.JUMP (e1, [label]) ->
                emit (A.OPER {assem="jmp `j0"; src=[]; dst=[]; jump=Some [label]})
        | T.EXP e ->
                let e = munchExp e in
                emit (A.MOVE {assem="mov `d0, `s0"; src=e; dst=e})
        | exp -> Print_tree.print exp; failwith "Compiler - unhandled munchStm case"
    and munchArgs (idx, arglist) =
        match arglist with
        | [] -> []
        | hd :: tl ->
            match hd with
            | T.CONST c ->
                    emit (A.OPER {assem="push " ^ (itos c); src=[Frame_x86.esp]; dst=[Frame_x86.esp]; jump=None});
                    munchArgs(0, tl)
            | T.NAME lab ->
                    emit (A.OPER {assem="push " ^ (Symbol.name lab); src=[Frame_x86.esp]; dst=[Frame_x86.esp]; jump=None});
                    munchArgs(0, tl)
            | _ ->
                let tloc = munchExp hd in
                emit (A.OPER {assem="push `s0"; src=[tloc; Frame_x86.esp]; dst=[Frame_x86.esp]; jump=None});
                tloc :: munchArgs (0, tl)
    and binopExp (op, e1, e2) =
        let opstr = match op with
            | T.PLUS -> "add"
            | T.MINUS -> "sub"
            | T.MUL -> "imul"
            | _ -> failwith "Op not implemented yet"
        in
        match (e1, e2) with
        | (T.TEMP e1, T.CONST c2) ->
            result (fun r ->
                emit (A.MOVE {assem="mov `d0, `s0"; dst=r; src=e1});
                emit (A.OPER {assem=opstr ^ " `d0, " ^ (itos c2); dst=[r]; src=[r]; jump=None})
            )
        | (e1, e2) ->
            result (fun r ->
                let e2 = munchExp e2
                and e1 = munchExp e1 in
                emit (A.MOVE {assem="mov `d0, `s0"; dst=r; src=e1});
                emit (A.OPER {assem=opstr ^ " `d0, `s0"; dst=[r]; src=[e2; r]; jump=None})
            )
    and munchExp : Tree.exp -> Temp.temp = function
        | T.TEMP t -> t
        | T.NAME lab ->
                result (fun r ->
                    emit (A.OPER {assem="mov `d0, " ^ (Symbol.name lab); src=[]; dst=[r]; jump=None})
                )
        | T.CALL (T.NAME lab, args) ->
                (* Hack for external C calls *)
                emit (A.OPER {assem = "CALL " ^ (Symbol.name lab);
                              src = munchArgs (0, args);
                              (* dst should have any registers that get clobbered by
                               * the assem instruction output about *)
                              dst = (*calldefs*)[Frame_x86.eax]; (* registers that get trashed *)
                              jump = None});
                (* Need to emit the cleanup of the stack *)
                emit (A.OPER {assem="add `s0, " ^ (itos ((List.length args)*4));
                              src=[Frame_x86.esp]; dst=[Frame_x86.esp]; jump=None});
                Frame_x86.eax

        (*| T.LABEL lab ->
                result (fun r ->
                    emit (T.OPER {assem="mov `d0, " ^ (Symbol.name lab); src=[]; dst=[r]; jump=None})
                )
                *)
        | T.MEM (T.BINOP (T.PLUS, T.TEMP e1, T.CONST c)) ->
            result (fun r ->
                emit (A.MOVE {assem="mov `d0, [`s0+" ^ (itos c) ^ "]"; src=e1; dst=r})
            )
        | T.MEM (T.TEMP e1) ->
            result (fun r ->
                emit (A.MOVE {assem="mov `d0, [`s0]"; src=e1; dst=r})
            )
        | T.MEM e -> result (fun r -> emit (A.MOVE {assem="mov `d0, [`s0]"; src=munchExp e; dst=r}))
        (* TODO these two cases are not quite correct *)
        (*
        | T.BINOP (T.MINUS, e1, T.CONST c) ->
            let loc = munchExp e1 in
            emit (A.OPER {assem="sub `d0, " ^ (itos c); src=[]; dst=[loc]; jump=None});
            loc
        | T.BINOP (T.PLUS, e1, T.CONST c) ->
            let loc = munchExp e1 in
            emit (A.OPER {assem="add `d0, " ^ (itos c); src=[]; dst=[loc]; jump=None});
            loc
            *)
        | T.CONST i -> result (fun r ->
            emit (A.OPER {assem="mov `d0, " ^ (itos i); src=[]; dst=[r]; jump=None}))
        | T.BINOP (op, e1, e2) -> binopExp (op, e1, e2)
        (*
        | T.BINOP (T.MUL, e1, e2) ->
            result (fun r ->
                let e2 = munchExp e2
                and e1 = munchExp e1 in
                emit (A.MOVE {assem="mov `d0, `s0"; dst=r; src=e1});
                emit (A.OPER {assem="imul `d0, `s0"; dst=[r]; src=[e2; r]; jump=None})
            )
        | T.BINOP (T.PLUS, e1, e2) ->
            result (fun (r : Temp.temp) ->
                let e2 = munchExp e2
                and e1 = munchExp e1 in
                emit (A.MOVE {assem="mov `d0, `s0"; dst=r; src=e1});
                emit (A.OPER {assem="add `d0, `s0";
                 dst=[r];
                 src=[e2; r]; jump=None}))
        *)
        | exp -> Print_tree.print (T.EXP exp); failwith "Compiler - unhandled munchExp case"
    in
    List.iter munchStm stmlist;
    Frame_x86.procEntryExit2 (frame, List.rev !ilist)
;;
