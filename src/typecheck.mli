open Env
open Translate

module type Typecheck = functor (E : Env) (T : Translate) ->
sig

type venv = E.enventry Symbol.table
type tenv = Types.ty Symbol.table

exception Error
exception Impossible

type expty = T.exp * Types.ty

type pos = Absyn.pos

val transProg : pos Absyn.exp -> expty

val transVar : venv * tenv * T.level * Temp.label option * pos Absyn.var -> expty
val transExp : venv * tenv * T.level * Temp.label option * pos Absyn.exp -> expty
val transDec : venv * tenv * T.level * Temp.label option * pos Absyn.dec -> 
               (venv * tenv * T.exp list)
val transTy  :         tenv * pos Absyn.ty  -> Types.ty

end

module Make : functor (E : Env) (T : Translate) -> Typecheck
