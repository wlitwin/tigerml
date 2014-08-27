type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

exception Error
exception Impossible

type expty = Translate.exp * Types.ty

type pos = Absyn.pos

val transProg : pos Absyn.exp -> expty

val transVar : venv * tenv * Translate.level * Temp.label option * pos Absyn.var -> expty
val transExp : venv * tenv * Translate.level * Temp.label option * pos Absyn.exp -> expty
val transDec : venv * tenv * Translate.level * Temp.label option * pos Absyn.dec -> 
               (venv * tenv * Translate.exp list)
val transTy  :        tenv * pos Absyn.ty  -> Types.ty
