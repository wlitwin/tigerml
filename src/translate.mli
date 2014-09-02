type level
type access (* Not the same as Frame.access *)

type exp

val outermost : level
             (* parent   name          formals *)
val newLevel : level -> Temp.label -> bool list -> level
val formals : level -> access list
val allocLocal : level -> bool -> access

val print : exp -> unit

(* Translation functions *)
val simpleVar : access -> level -> exp
val fieldVar : exp -> (Symbol.symbol * Types.ty) list -> Symbol.symbol -> exp
val subscriptVar : exp -> exp -> exp

val procEntryExit : level -> exp -> unit
val getResult : unit -> Frame.frag list

val intExp : int -> exp
val nilExp : unit -> exp
val strExp : string -> exp
val ifExp : exp -> exp -> exp -> exp
val seqExp : exp list -> exp
val recordExp : exp list -> exp
val assignExp : exp -> exp -> exp
val whileExp : exp -> exp -> Temp.label -> exp
val breakExp : Temp.label -> exp
val arrayExp : exp -> exp -> exp
val callExp : Temp.label -> exp list -> level -> level -> exp

val arithExp : Absyn.oper -> exp -> exp -> exp
val condExp : Absyn.oper -> exp -> exp -> exp
