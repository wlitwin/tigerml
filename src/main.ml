open Lexing
open Tree
open Canon
open Assem
open Graph

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
        (*Print_ast.print res;*)
        (* Find escapes *)
        Findescape.findescape res;
        (* Typecheck the program *)
        let (ir, typ) = Typecheck.transProg res in
        print_endline ("Final program type: " ^ (Types.str typ)); 
        Translate.print ir;
        print_endline "Successfully typechecked";
        (* Linearize, basic blocks, and traces *)
        (*let linear = Canon.linearize (Tree.EXP ir) in
        let (blocks, exit_label) = Canon.basicBlocks linear in
        let trace = Canon.traceSchedule (blocks, exit_label) in 
*)
        ()
    with
    | ErrorMsg.Error -> ()
    | Tiger.Error -> 
            let s_pos = Lexing.lexeme_start_p lexbuf in
            let e_pos = Lexing.lexeme_end_p lexbuf in
            Printf.eprintf "Parse error in %s\n  line: %d [%d:%d]\n  Token: %s\n" 
                s_pos.pos_fname 
                s_pos.pos_lnum 
                (s_pos.pos_cnum - s_pos.pos_bol)
                (e_pos.pos_cnum - e_pos.pos_bol)
                (Lexing.lexeme lexbuf)
    | Typecheck.Error -> ()
    ;
    close_in input;
    ()
;;
