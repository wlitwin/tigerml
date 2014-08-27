type unique = unit ref

type ty = RECORD of (Symbol.symbol * ty) list * unique
        | NIL
        | INT
        | STRING
        | ARRAY of ty * unique
        | NAME of Symbol.symbol * ty option ref
        | UNIT

let rec str ty =
    match ty with
    | NIL -> "nil"
    | INT -> "int"
    | UNIT -> "unit"
    | STRING -> "string"
    | RECORD (lst, _) -> 
            "{" ^ (String.concat "; " 
                (List.map (fun (s, t) -> 
                    (Symbol.name s) ^ "=" ^ (str t)) lst)) ^ "}"
    | ARRAY (ty, _) -> "array of " ^ (str ty) 
    | NAME (sym, ref) -> 
            (Symbol.name sym)(* ^ " = " ^ 
                (match !ref with
                | Some ty -> str ty
                | None -> "<unknown type>")
                *)
            
;;
