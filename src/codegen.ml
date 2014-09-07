module A = Assem
module S = Symbol
module T = Tree

let codegen frame (stm : Tree.stm) : Assem.instr list =
    let ( $ ) a b c = b (a c)
    and ( $! ) a b = a b in
    let stmlist = let open Canon in
        linearize $ basicBlocks $ traceSchedule $! stm
    in
    let ilist = ref ([] : A.instr list) in
    let emit x = ilist := x :: !ilist in
    let result gen = 
        let t = Temp.newtemp () in
        gen t; t
    in
    let itos = string_of_int in
    let open Assem in
    let rec munchStm : T.stm -> unit = function
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2) ->
                emit (A.OPER {assem = "mov " ^ (itos i) ^ "('s1)\n";
                              src = [munchExp e1; munchExp e2];
                              dst = []; jump = None })
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2) ->
                emit (A.OPER {assem = "mov " ^ (itos i) ^ "('s1)\n";
                              src = [munchExp e1; munchExp e2];
                              dst = []; jump = None })
        | T.MOVE (T.MEM e1, T.MEM e2) ->
                emit (A.OPER {assem = "mov 's0, 's1\n";
                              src = [munchExp e1; munchExp e2];
                              dst = []; jump = None})
        | T.MOVE (T.MEM (T.CONST i), e2) ->
                emit (A.OPER {assem = "mov (" ^ itos i ^ ")r0, s0\n";
                              src = [munchExp e2];
                              dst = []; jump = None})
        | T.MOVE (T.MEM e1, e2) ->
                emit (A.OPER {assem = "mov ('s0), 's1\n";
                              src = [munchExp e1; munchExp e2];
                              dst = []; jump = None})
        | T.MOVE (T.TEMP i, e2) ->
                emit (A.OPER {assem = "add 'd0, 's0, 'r0\n";
                              src = [munchExp e2];
                              dst = [i]; jump = None})
        | T.LABEL lab ->
                emit (A.LABEL {assem = (S.name lab) ^ ":\n"; lab})
        | T.EXP (T.CALL (e, args)) ->
                emit (A.OPER {assem = "CALL 's0\n";
                              src = munchExp e :: munchArgs (0, args);
                              (* dst should have any registers that get clobbered by
                               * the assem instruction output about *)
                              dst = (*calldefs*)[]; (* registers that get trashed *)
                              jump = None})
        | _ -> failwith "Compiler - unhandled munchStm case"
    and munchArgs (idx, arglst) =
        []
    and munchExp = function
        | T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)) ->
                result (fun r -> emit (A.OPER
                    {assem = "LOAD 'd0 <- M['s0 + " ^ itos i ^ "]\n";
                     src = [munchExp e1]; dst = [r]; jump = None }))
        | T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)) -> 
                result (fun r -> emit (A.OPER
                    {assem = "LOAD 'd0 <- M['s0 + " ^ itos i ^ "]\n";
                     src = [munchExp e1]; dst = [r]; jump = None }))
        | T.MEM (T.CONST i) ->
                result (fun r -> emit (A.OPER
                    {assem = "LOAD 'd0 <- M[r0 + " ^ itos i ^ "]\n";
                     src = []; dst = [r]; jump = None }))
        | T.MEM e1 ->
                result (fun r -> emit (A.OPER
                    {assem = "LOAD 'd0 <- M['s0+0]\n";
                     src = [munchExp e1]; dst = [r]; jump = None}))
        | T.BINOP (T.PLUS, T.CONST i, e1) ->
                result (fun r -> emit (A.OPER
                    {assem = "ADDI 'd0 <- 's0 + " ^ itos i ^ "\n";
                     src = [munchExp e1]; dst = [r]; jump = None}))
        | T.BINOP (T.PLUS, e1, T.CONST i) ->
                result (fun r -> emit (A.OPER
                    {assem = "ADDI 'd0 <- 's0 + " ^ itos i ^ "\n";
                     src = [munchExp e1]; dst = [r]; jump = None}))
        | T.CONST i ->
                result (fun r -> emit (A.OPER
                    {assem = "ADDI 'd0 <- r0 + " ^ itos i ^ "\n";
                     src = []; dst = [r]; jump = None}))
        | T.BINOP (T.PLUS, e1, e2) ->
                result (fun r -> emit (A.OPER
                    {assem = "ADD 'd0 <- 's0+'s1\n";
                     src = [munchExp e1; munchExp e2];
                     dst = [r]; jump = None}))
        | T.TEMP t -> t
        | _ -> failwith "Compiler - unhandled munchExp case"
    in
    List.iter munchStm stmlist;
    List.rev !ilist
;;
