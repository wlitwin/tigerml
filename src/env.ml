module type T = 
    functor (F : Frame.Frame) (T : (module type of Translate.Make (F))) ->
sig
    type access = enventry
    and ty = Types.ty
    and enventry = VarEntry of T.access * ty
                 | FunEntry of T.level * (* Level *)
                               Temp.label * (* Label *)
                               ty list * (* Formals *)
                               ty (* Result *)

    type tenv = ty Symbol.table
    type venv = enventry Symbol.table

    val base_venv  : unit -> venv
    val base_tenv  : unit -> tenv
    val print_venv : venv -> unit
    val print_tenv : tenv -> unit
end

module Make : T = functor (F : Frame.Frame) (T : module type of Translate.Make (F)) ->
struct

type access = enventry
and ty = Types.ty
and enventry = VarEntry of T.access * ty
             | FunEntry of T.level * (* Level *)
                           Temp.label * (* Label *)
                           ty list * (* Formals *)
                           ty (* Result *)

type tenv = ty Symbol.table
type venv = enventry Symbol.table

let add_builtin tbl name value =
    Symbol.enter (tbl, Symbol.symbol name, value) 
;;

let base_tenv () : ty Symbol.table = 
    let empty = Symbol.empty () in
    let tbl = add_builtin empty "int" Types.INT in
    add_builtin tbl "string" Types.STRING
;;

let base_venv () : enventry Symbol.table =
    let open Types in
    let empty = Symbol.empty () in
    (*
    let tbl = add_builtin empty "print" (FunEntry ([STRING], UNIT)) in
    let tbl = add_builtin tbl "flush" (FunEntry ([], UNIT)) in
    let tbl = add_builtin tbl "getchar" (FunEntry ([], STRING)) in
    let tbl = add_builtin tbl "ord" (FunEntry ([STRING], INT)) in
    let tbl = add_builtin tbl "chr" (FunEntry ([INT], STRING)) in
    let tbl = add_builtin tbl "size" (FunEntry ([STRING], INT)) in
    let tbl = add_builtin tbl "substring" (FunEntry ([STRING; INT; INT], STRING)) in
    let tbl = add_builtin tbl "concat" (FunEntry ([STRING; STRING], STRING)) in
    let tbl = add_builtin tbl "not" (FunEntry ([INT], INT)) in
    let tbl = add_builtin tbl "exit" (FunEntry ([], UNIT)) in
    tbl
*)
    empty
    
;;

let print_venv venv =
    let pval v =
        match v with
        | VarEntry (_, ty) -> Types.str ty
        | FunEntry (_, _, formals, body) ->
                "(" ^ (String.concat ", " 
                    (List.map Types.str formals)) ^ ") : " ^
                    (Types.str body)
    in
    print_string "VENV ";
    Symbol.print pval venv
;;

let print_tenv tenv =
    print_string "TENV ";
    Symbol.print Types.str tenv
;;

end
