open Typecheck

module type FindEscape =
sig
    val findescape: 'a Absyn.exp -> unit
end

module Make :
    functor (T : Typecheck) -> FindEscape
