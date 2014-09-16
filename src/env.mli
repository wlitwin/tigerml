module type T = functor 
    (F : Frame.Frame)
    (T : (module type of Translate.Make (F))) ->
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

module Make : T
