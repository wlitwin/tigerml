(* Holds location of all formals
 * Instructions for the view-shift
 * Number of locals allocated so far
 * label at which the functions code begins
 *)
type frame
type access

type frag = STRING of Temp.label * string
          | FUNCTION of Tree.stm * frame

val fp : Temp.temp (* Frame pointer loction *)
val rv : Temp.temp (* Return value location *)
val wordsize : int
val exp : access -> Tree.exp -> Tree.exp

val procEntryExit1 : frame * Tree.stm -> Tree.stm

            (* func name    escape list *)
val newFrame : Temp.label -> bool list -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val allocLocal : frame -> bool -> access
val externalCall : string * Tree.exp list -> Tree.exp
val addfragment : frag -> unit
