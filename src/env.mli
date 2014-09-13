module type Env =
sig
    type t_access
    type t_level

    type access = enventry
    and ty = Types.ty
    and enventry = VarEntry of t_access * ty
                 | FunEntry of t_level * (* Level *)
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

module Make : functor (T : Translate.Translate) -> Env with type t_access = T.access
                                                       with type t_level  = T.level
