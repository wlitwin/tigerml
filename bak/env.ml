module type T = 
    functor (F : Frame.Frame) (T : (module type of Translate.Make (F))) ->
sig
    type access = enventry
    and ty = Types.ty
    and enventry = VarEntry of T.access
                 | FunEntry of T.level * (* Level *)
                               Temp.label (* Label *)
                 | ExtFunEntry of T.level *
                                  Temp.label 

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
and enventry = VarEntry of T.access
             | FunEntry of T.level * (* Level *)
                           Temp.label (* Label *)
             | ExtFunEntry of T.level *
                              Temp.label

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
    List.fold_left (fun env (name, args, ret) ->
        add_builtin env name (ExtFunEntry (level, Temp.namedlabel name))
    ) empty Builtin.stdlib_funs;
;;

let print_venv venv =
    print_endline "REIMPLEMENT!@#!@#!@#!@#"
    (*
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
    *)
;;

let print_tenv tenv =
    print_string "TENV ";
    Symbol.print Types.str tenv
;;

end
