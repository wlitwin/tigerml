module type Codegen = functor (F : Frame.Frame) ->
sig
    val codegen : F.frame -> Tree.stm -> Assem.instr list
end

module Make : Codegen
