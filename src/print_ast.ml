module A = Absyn

let print tree =
    let say s : unit = print_string s in
    let sayln s : unit = say s; say "\n" in
    let rec indent i : unit = 
        match i with
        | 0 -> ()
        | i -> say " "; indent (i - 1)
    in
    let opname = function
        | A.PlusOp -> "PlusOp"
        | A.MinusOp -> "MinusOp"
        | A.TimesOp -> "TimesOp"
        | A.DivideOp -> "DivideOp"
        | A.EqOp -> "EqOp"
        | A.NeqOp -> "NeqOp"
        | A.LtOp -> "LtOp"
        | A.LeOp -> "LeOp"
        | A.GtOp -> "GtOp"
        | A.GeOp -> "GeOp"
    in
    let rec dolist d f lst : unit = 
        match lst with
        | [a] -> sayln ""; f (a, d+1)
        | (a::r) -> sayln ""; f (a, d+1); say ","; dolist d f r
        | [] -> ()
    in
    let rec var v : unit = 
        match v with
        | (A.SimpleVar (s, _), d) -> 
                 indent d; say "SimpleVar("; say (Symbol.name s); say ")"
        | (A.FieldVar (v, s, _), d) -> 
                 indent d; sayln "FieldVar("; var (v, d+1); 
                 sayln ","; indent (d+1); say (Symbol.name s); say ")"
        | (A.SubscriptVar (v, e, _), d) -> 
                 indent d; sayln "SubscriptVar("; 
                 var (v, d+1); sayln ",";
                 exp (e, d+1); 
                 say ")"
    and exp e : unit =
        match e with
        | (A.VarExp (v, _), d) -> 
                indent d; sayln "VarExp("; var (v, d+1); say ")"
        | (A.NilExp _, d) -> 
                indent d; say "NilExp"
        | (A.IntExp (i, _), d) -> 
                indent d; say "IntExp("; say (string_of_int i); say ")"
        | (A.StringExp (s, p), d) ->
                indent d; say "StringExp(\""; say s; say "\")"
        | (A.CallExp ((cfunc, cargs), _), d) ->
                 indent d; say "CallExp("; say (Symbol.name cfunc); say ",[";
                 dolist d exp cargs; say "])"
        | (A.OpExp ((oleft, oper, oright), _), d) ->
                 indent d; say "OpExp("; say (opname oper); sayln ",";
                 exp (oleft, d+1); sayln ","; exp (oright, d+1); say ")"
        | (A.RecordExp ((rfields, rtyp), _), d) ->
                 let f ((name, e), d) =
                     indent d; say "("; say (Symbol.name name); sayln ",";
                     exp (e, d+1); say ")"
                 in
                 indent d; say "RecordExp("; say (Symbol.name rtyp);
                 sayln ",["; 
                 dolist d f rfields; 
                 say "])"
        | (A.SeqExp (l, _), d) ->
                 indent d; say "SeqExp["; dolist d exp l; say "]"
        | (A.AssignExp ((v, e), _), d) ->
                 indent d; sayln "AssignExp("; var (v, d+1); sayln ",";
                 exp (e, d+1); say ")"
        | (A.IfExp ((test, _then, _else), _), d) ->
                 (indent d; sayln "IfExp("; exp (test, d+1); sayln ",";
                 exp (_then, d+1); 
                 match _else with Some e -> (sayln ","; exp (e, d+1))
                 | None -> ();
                 say ")")
        | (A.WhileExp ((test, body), _), d) ->
                 indent d; sayln "WhileExp("; exp (test, d+1); sayln ",";
                 exp (body, d+1); say ")"
        | (A.ForExp ((v, b, lo, hi, body), _), d) ->
                 indent d; say "ForExp("; say (Symbol.name v); say ",";
                 say (string_of_bool !b); sayln ","; indent (d+1); exp (lo, d+1);
                 sayln ","; exp (hi, d+1); sayln ","; indent (d+1); exp (body, d+1);
                 say ")"
        | (A.BreakExp _, d) -> indent d; say "BreakExp"
        | (A.LetExp ((decs, body), _), d) ->
                 indent d; say "LetExp(["; dolist d dec decs; sayln "],";
                 List.iter (fun body -> exp (body, d+1)) body; say ")"
        | (A.ArrayExp ((typ, size, init), _), d) ->
                 indent d; say "ArrayExp("; say (Symbol.name typ); sayln ",";
                 exp (size, d+1); sayln ","; exp (init, d+1); say ")"
    and dec t : unit = 
        match t with
        | (A.FunctionDec (l, _), d) ->
                let field p = 
                    let (((name, escape, typ), _), d) = p in
                    indent d; say "("; say (Symbol.name name); say ",";
                    say (string_of_bool !escape); say ","; say (Symbol.name typ);
                    say ")"
                in
                let f p = 
                    let (((name, params, result, body), _), d) = p in
                    indent d; say "("; say (Symbol.name name); say ",[";
                    dolist d field params; sayln "],"; indent (d+1);
                    match result with 
                    | Some (s, _) -> (say "SOME("; say (Symbol.name s); say ")")
                    | None -> say "NONE";
                    sayln ","; exp (body, d+1); say ")"
                in
                indent d; say "FunctionDec["; dolist d f l; say "]"
        | (A.VarDec ((name, escape, typ, init), _), d) ->
                (indent d; say "VarDec("; say (Symbol.name name); say ",";
                 say (string_of_bool !escape); say ",";
                 match typ with None -> say "NONE"
                 | Some (s, p) -> (say "SOME("; say (Symbol.name s); say ")");
                 sayln ","; exp (init, d+1); say ")")
        | (A.TypeDec (l, _), d) ->
                let tdec p = 
                    let ((name, t, _), d) = p in
                    indent d; say "("; say (Symbol.name name); sayln ",";
                    ty (t, d+1); say ")"
                in
                indent d; say "TypeDec["; 
                dolist d tdec l; say "]"
    and ty = function 
        | (A.NameTy (s, _), d) ->
                indent d; say "NameTy("; say (Symbol.name s); say ")"
        | (A.RecordTy (l, _), d) ->
                let f p = 
                    let (((name, escape, typ), _), d) = p in
                    indent d; say "("; say (Symbol.name name); say ",";
                    say (string_of_bool !escape); say ","; 
                    say (Symbol.name typ); say ")"
                in
                indent d; say "RecordTy["; dolist d f l; say "]"
        | (A.ArrayTy (s, _), d) ->
                indent d; say "ArrayTy("; say (Symbol.name s); say ")"
    in
    exp (tree, 0); sayln "";
;;
