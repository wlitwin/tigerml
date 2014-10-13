(* Holds location of all formals
 * Instructions for the view-shift
 * Number of locals allocated so far
 * label at which the functions code begins
 *)
module type Frame = 
sig

type frame
type access
type register

val fp : Temp.temp (* Frame pointer loction *)
val rv : Temp.temp (* Return value location *)
val registers : register list
val tempMap: register Temp.ITable.table
val wordsize : int

val externalCall : string * Tree.exp list -> Tree.exp

type frag = STRING of Temp.label * string
          | FUNCTION of Tree.stm * frame

val exp : access -> Tree.exp -> Tree.exp

val procEntryExit1 : frame * Tree.stm -> Tree.stm
val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

type pe3_rec = {prolog: string; body: Assem.instr list; epilog: string}
val procEntryExit3 : frame * Assem.instr list -> pe3_rec
                     
val string_of_temp : Temp.temp -> string

            (* func name    escape list *)
val newFrame : Temp.label -> bool list -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val allocLocal : frame -> bool -> access

end
