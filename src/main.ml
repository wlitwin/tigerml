module Fx86 = Frame_x86
module Cx86 = Codegen_x86
module Tx86 = Translate.Make (Fx86)
module Ex86 = Env.Make (Fx86) (Tx86)
module Sx86 = Semant.Make (Fx86) (Tx86) (Ex86)

module RAx86 = Regalloc.Make(Fx86)
(*module GCx86 = Color.Make(Fx86)*)

open Table
open Graph
open Flowgraph
open Makegraph
open Liveness
(*open Color*)
open Gcolor

module A = Assem

let genProgram (fragList : Fx86.frag list) : unit =
    (*SEQ(
        * ---+---+MOVE(
            * ---+---+-TEMP t0,
            * ---+---+-ESEQ(
                * ---+---+--MOVE(
                    * ---+---+---MEM(
                        * ---+---+---+BINOP(PLUS,
                        * ---+---+---+-TEMP t5,
                        * ---+---+---+-CONST -16)),
                        * ---+---+---CONST 12),
                        * ---+---+--ESEQ(
                            * ---+---+---MOVE(
                                * ---+---+---+MEM(
                                    * ---+---+---+-BINOP(PLUS,
                                    * ---+---+---+--TEMP t5,
                                    * ---+---+---+--CONST -16)),
                                    * ---+---+---+CALL(
                                        * ---+---+---+-NAME L1,
                                        * ---+---+---+--CONST 0,
                                        * ---+---+---+--CONST 20)),
                                        * ---+---+---CONST 0))),
                                        * *)
    (*
    let module T = Tree in
    let t1 = Temp.newtemp()
    and t2 = Temp.newtemp() in
    let tree = T.MOVE (T.TEMP t1,
                       T.ESEQ (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP t2, T.CONST ~-16)), T.CONST 12),
                               T.ESEQ (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP t2, T.CONST ~-16)),
                                              T.CALL (T.NAME (Temp.namedlabel "blah"), [T.CONST 0; T.CONST 20])),
                                       T.CONST 0)))
    in
    Print_tree.print tree;
    (*
    let stms = Canon.linearize tree in
    List.iter (fun s ->
        Print_tree.print s;
    ) stms;
    *)
    let frame = Fx86.newFrame (Temp.namedlabel "main") [] in
    let instrs = Cx86.codegen frame tree in
    List.iter (fun i ->
        print_endline (Assem.format Fx86.string_of_temp i)
    ) instrs;
    *)
    let strlst : string list ref = ref [] in
    let append s = strlst := !strlst @ [s] in
    List.iter (fun frag ->
        print_endline "---- TRANSLATING FRAGMENT ----";
        match frag with
        | Fx86.STRING (label, str) ->
            append(Fx86.genString(label, str));
            print_endline ("STRING: " ^ (Symbol.name label) ^ " " ^ str)
        | Fx86.FUNCTION (stm, frame) ->
            Print_tree.print stm;     
            let instr = Cx86.codegen frame stm
            (*
                let open Assem in
                let a = Temp.newtemp()
                and b = Temp.newtemp()
                and c = Temp.newtemp() in
                let lab = Temp.newlabel() in
                let lab2 = Temp.newlabel() in
                (OPER{assem="mov `d0, 0"; dst=[a]; src=[]; jump=None}) ::
                (OPER{assem="mov `d0, 0"; dst=[c]; src=[]; jump=None}) ::
                (LABEL{assem=(Symbol.name lab); lab}) ::
                (OPER{assem="add `d0, `s0, 1"; dst=[b]; src=[a]; jump=None}) ::
                (OPER{assem="add `d0, `s0, `s1"; dst=[c]; src=[c;b]; jump=None}) ::
                (OPER{assem="mul `d0, `s0, 2"; dst=[a]; src=[b]; jump=None}) ::
                (OPER{assem="le `s0, N"; dst=[]; src=[a]; jump=Some[lab;lab2]}) ::
                (LABEL{assem=(Symbol.name lab); lab=lab2}) ::
                (OPER{assem="ret `s0"; dst=[]; src=[c]; jump=None}) ::
                []
    *)
            in
            List.iteri (fun idx i ->
               (* Assem.print_instr i;*)
                print_string ((string_of_int idx) ^ " ");
                print_endline (Assem.format Fx86.string_of_temp i)
            ) instr;
            let (fgraph, nodes) = Makegraph.instrs2graph instr in
            let (igraph, table) = Liveness.interferenceGraph fgraph in
            print_endline "--- LIVENESS IGRAPH ----";
            Liveness.show (stdout, igraph);
            print_endline "--- GRAPH COLORING ----";
            let (cgraph, colors : Liveness.Graph.graph * int Temp.ITable.table) = Gcolor.color igraph Fx86.precolored Fx86.numRegisters in
            (*Liveness.Graph.show cgraph; *)
            print_endline "--- COLORS ---";
            (*
            Liveness.Graph.ITable.iter (fun k v -> 
                Liveness.Graph.show_node k;
                print_endline (string_of_int v);
            ) colors;
            *)
            print_endline "--- NEW ASSEM ---";
            let replaceTemp t =
                match Temp.ITable.look(colors, t) with
                | Some color -> color
                | None -> t
            in
            let makeNewInstr instr =
                let open Assem in
                match instr with
                | OPER {assem; dst; src; jump} ->
                        Some (OPER {assem; dst=List.map replaceTemp dst; src=List.map replaceTemp src; jump})
                | MOVE {assem; dst; src} ->
                        let newDst = replaceTemp dst
                        and newSrc = replaceTemp src in
                        (*if newDst = newSrc then None
                        else*) Some (MOVE {assem; dst=replaceTemp dst; src=replaceTemp src})
                | _ -> Some instr
            in
            let newInstrList = 
                List.fold_left (fun acc i ->
                    match makeNewInstr i with
                    | Some i -> i :: acc
                    | None -> acc
                ) [] instr
                |> List.rev
            in
            let result = Fx86.procEntryExit3 (frame, newInstrList) in
            print_endline result.Fx86.prolog;
                append(result.Fx86.prolog);
            List.iter (fun i ->
                let str = Assem.format Fx86.string_of_temp i in
                print_endline str;
                append (str);
            ) result.Fx86.body;
            print_endline result.Fx86.epilog;
                append(result.Fx86.epilog);
            ()
    ) fragList;

    (* Write assembly to file *)
    let out = open_out "prog.s" in
    Printf.fprintf out "%s"
        (String.concat "\n" 
        ["bits 32";"global __prog";"extern print";"extern ord";"extern chr";
         "extern substring";"extern getchar_";"extern flush";"extern size";
         "extern concat";"extern not";"extern allocRecord";"extern initArray";
         "extern blah";"extern stringEqual"]);
    List.iter (fun str ->
        Printf.fprintf out "%s\n" str
    ) !strlst;
    close_out out;
    (* Try to compile and run it *)
    print_endline ("Error code: " ^ (string_of_int (Sys.command("./compile_and_run.sh"))));
;;

let () =
    ErrorMsg.reset ();
    let input = 
        if Array.length Sys.argv > 1 then (
            ErrorMsg.fileName := Sys.argv.(1);
            open_in Sys.argv.(0)
        ) else (
            ErrorMsg.fileName := "stdin";
            stdin
        )
    in

    let lexbuf = Lexing.from_channel input in
    try
        let res = Tiger.program Lexer.token lexbuf in
        print_endline "Successfully parsed";
        Print_ast.print res;
        (* Find escapes *)
        Findescape.findescape res;
        (* Typecheck the program *)
        let fragments = Sx86.transProg res in
        print_endline "Successfully typechecked";
        genProgram fragments;
        (* Linearize, basic blocks, and traces *)
        (*let linear = Canon.linearize (Tree.EXP ir) in
        let (blocks, exit_label) = Canon.basicBlocks linear in
        let trace = Canon.traceSchedule (blocks, exit_label) in 
*)
        ()
    with
    | ErrorMsg.Error -> ()
    | Findescape.VarNotFound -> ()
    | Tiger.Error -> 
            let open Lexing in
            let s_pos = Lexing.lexeme_start_p lexbuf in
            let e_pos = Lexing.lexeme_end_p lexbuf in
            Printf.eprintf "Parse error in %s\n  line: %d [%d:%d]\n  Token: %s\n" 
                s_pos.pos_fname 
                s_pos.pos_lnum 
                (s_pos.pos_cnum - s_pos.pos_bol)
                (e_pos.pos_cnum - e_pos.pos_bol)
                (Lexing.lexeme lexbuf)
    (*| Typecheck.Error -> ()*)
    ;
    close_in input;
    ()
;;
