(*
module Env_x86 = Env.Make (Translate_x86)
*)
(*module Typecheck_x86 = Typecheck.Make (Env_x86) (Translate_x86)*)
module Fx86 = Frame_x86.Frame_x86
module Cx86 = Codegen.Make (Fx86)
module Tx86 = Translate.Make (Fx86)
module Ex86 = Env.Make (Fx86) (Tx86)
module Tyx86 = Typecheck.Make (Fx86) (Ex86) (Tx86)
(*module Tx86 = Make.Translate (Frame_x86.Frame_x86)*)

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
        (*let res = Tiger.program Lexer.token lexbuf in
        print_endline "Successfully parsed";
        *)
        (*Print_ast.print res;*)
        (* Find escapes *)
(*        Findescape.findescape res;
        (* Typecheck the program *)
        let (ir, typ) = Typecheck.transProg res in
        print_endline ("Final program type: " ^ (Types.str typ)); 
        Translate.print ir;
        print_endline "Successfully typechecked";
        *)
        (* Linearize, basic blocks, and traces *)
        (*let linear = Canon.linearize (Tree.EXP ir) in
        let (blocks, exit_label) = Canon.basicBlocks linear in
        let trace = Canon.traceSchedule (blocks, exit_label) in 
*)
        ()
    with
    | ErrorMsg.Error -> ()
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
