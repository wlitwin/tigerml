module type T = 
    functor(Frame : Frame.Frame) ->
sig
    type allocation = (Frame.register, Temp.temp) Hashtbl.t
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

module Make : T = functor (Frame : Frame.Frame) ->
struct
    type allocation = (Frame.register, Temp.temp) Hashtbl.t
    let alloc (instr, frame : Assem.instr list * Frame.frame) : Assem.instr list * allocation =
        ([], Hashtbl.create 10)
    ;;
end
