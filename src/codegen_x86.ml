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
    print_endline "---- BASIC BLOCKS START ----";
    let (blocks, lab) = Canon.basicBlocks (Canon.linearize stm) in
    List.iteri (fun i lst ->
        print_endline ("block " ^ (string_of_int i));
        List.iter (fun i -> 
            Print_tree.print i
        ) lst;
    ) blocks;
    print_endline "---- BASIC BLOCKS END ----";
    let stmlist = let open Canon in
        linearize $ basicBlocks $ traceSchedule $! stm
    in
    let ilist = ref ([] : A.instr list) in
    let emit x = ilist := x :: !ilist in
    let result gen : Temp.temp =
        let t = Temp.newtemp () in
        gen t; t
    in
    let itos = string_of_int in
    let open Assem in
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
        | T.MOVE (e1, T.CONST i) ->
                emit (A.OPER {assem="mov `d0, " ^ (itos i); 
                              dst=[munchExp e1]; 
                              src=[]; jump=None})
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c)), e2) ->
                emit (A.MOVE {assem="mov [`d0+" ^ (itos c) ^ "], `s0";
                              dst=munchExp e1;
                              src=munchExp e2;})
        | T.MOVE (e1, (T.MEM (T.BINOP (T.PLUS, e2, T.CONST c)))) ->
                emit (A.MOVE {assem="mov `d0, [`s0+" ^ (itos c) ^ "]";
                              dst=munchExp e1;
                              src=munchExp e2;})
        | T.MOVE (e1, e2) ->
                emit (A.MOVE {assem="mov `d0, `s0"; dst=munchExp e1; src=munchExp e2})
        | T.LABEL lab ->
                emit (A.LABEL {assem = (S.name lab) ^ ":"; lab})
        | T.JUMP (e1, [label]) ->
                emit (A.OPER {assem="jmp `j0"; src=[]; dst=[]; jump=Some [label]})
        | T.EXP e ->
                let e = munchExp e in
                emit (A.MOVE {assem="mov `d0, `s0"; dst=e; src=e;})
        | exp -> Print_tree.print exp; failwith "Compiler - unhandled munchStm case"
    and munchArgs (idx, arglist) =
        match arglist with
        | [] -> []
        | hd :: tl ->
            match hd with
            | T.CONST c -> 
                    emit (A.OPER {assem="push " ^ (itos c); src=[]; dst=[]; jump=None});
                    munchArgs(0, tl)
            | _ ->
                let tloc = munchExp hd in
                emit (A.OPER {assem="push `s0"; src=[tloc]; dst=[]; jump=None});
                tloc :: munchArgs (0, tl)
    and munchExp : Tree.exp -> Temp.temp = function
        | T.TEMP t -> t
        | T.NAME lab ->
                result (fun r ->
                    emit (A.OPER {assem="mov `d0, " ^ (Symbol.name lab); src=[]; dst=[r]; jump=None})
                )
        | T.CALL (T.NAME lab, args) ->
                emit (A.OPER {assem = "CALL " ^ (Symbol.name lab);
                              src = munchArgs (0, args);
                              (* dst should have any registers that get clobbered by
                               * the assem instruction output about *)
                              dst = (*calldefs*)[Frame_x86.eax]; (* registers that get trashed *)
                              jump = None});
                (* Need to emit the cleanup of the stack *)
                emit (A.OPER {assem="add esp, " ^ (itos ((List.length args)*4)); 
                              src=[]; dst=[Frame_x86.esp]; jump=None});
                Frame_x86.eax
                
        (*| T.LABEL lab -> 
                result (fun r ->
                    emit (T.OPER {assem="mov `d0, " ^ (Symbol.name lab); src=[]; dst=[r]; jump=None})
                )
                *)
        | T.CONST i -> result (fun r ->
            emit (A.OPER {assem="mov `d0, " ^ (itos i); src=[]; dst=[r]; jump=None}))
        | T.BINOP (T.PLUS, e1, T.CONST c) ->
            let loc = munchExp e1 in
            emit (A.OPER {assem="add `d0, " ^ (itos c); src=[]; dst=[loc]; jump=None});
            loc
        | T.BINOP (T.MUL, e1, e2) ->
            result (fun r ->
                let e1 = munchExp e1
                and e2 = munchExp e2 in
                emit (A.MOVE {assem="mov `d0, `s0"; dst=r; src=e1});
                emit (A.OPER {assem="imul `d0, `s0"; dst=[r]; src=[e2; r]; jump=None})
            )
        | T.BINOP (T.PLUS, e1, e2) ->
            result (fun (r : Temp.temp) -> 
                let e1 = munchExp e1
                and e2 = munchExp e2 in
                emit (A.MOVE {assem="mov `d0, `s0"; dst=r; src=e1});
                emit (A.OPER {assem="add `d0, `s0";
                 dst=[r]; 
                 src=[e2; r]; jump=None}))
        | exp -> Print_tree.print (T.EXP exp); failwith "Compiler - unhandled munchExp case"
    in
    List.iter munchStm stmlist;
    Frame_x86.procEntryExit2 (frame, List.rev !ilist)
;;
