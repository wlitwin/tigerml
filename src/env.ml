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
                 | ExtFunEntry of T.level *
                                  Temp.label * 
                                  ty list *
                                  ty

    type tenv = ty Symbol.table
    type venv = enventry Symbol.table

    val base_venv  : T.level -> venv
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
             | ExtFunEntry of T.level *
                              Temp.label * 
                              ty list *
                              ty

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

let base_venv level : enventry Symbol.table =
    let open Types in
    let empty = Symbol.empty () in
    let tbl = add_builtin empty "print"   (ExtFunEntry (level, Temp.namedlabel "print", [STRING], UNIT)) in
    let tbl = add_builtin tbl   "flush"   (ExtFunEntry (level, Temp.namedlabel "flush", [], UNIT)) in
    let tbl = add_builtin tbl   "getchar" (ExtFunEntry (level, Temp.namedlabel "getchar_", [], STRING)) in
    let tbl = add_builtin tbl   "ord"     (ExtFunEntry (level, Temp.namedlabel "ord", [STRING], INT)) in
    let tbl = add_builtin tbl   "chr"     (ExtFunEntry (level, Temp.namedlabel "chr", [INT], STRING)) in
    let tbl = add_builtin tbl   "size"    (ExtFunEntry (level, Temp.namedlabel "size", [STRING], INT)) in
    let tbl = add_builtin tbl "substring" (ExtFunEntry (level, Temp.namedlabel "substring", [STRING; INT; INT], STRING)) in
    let tbl = add_builtin tbl   "concat"  (ExtFunEntry (level, Temp.namedlabel "concat", [STRING; STRING], STRING)) in
    let tbl = add_builtin tbl   "not"     (ExtFunEntry (level, Temp.namedlabel "not", [INT], INT)) in
    let tbl = add_builtin tbl   "exit"    (ExtFunEntry (level, Temp.namedlabel "exit", [], UNIT)) in
    let tbl = add_builtin tbl   "blah"    (ExtFunEntry (level, Temp.namedlabel "blah", [], STRING)) in
    let tbl = add_builtin tbl "stringEqual" (ExtFunEntry (level, Temp.namedlabel "stringEqual", [STRING; STRING], INT)) in
    tbl
;;

let print_venv venv =
    let pval v =
        match v with
        | VarEntry (_, ty) -> Types.str ty
        | ExtFunEntry (_, _, formals, body)
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
