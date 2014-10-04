module type T =
    functor (Frame : Frame.Frame) ->
sig
    type allocation = (Frame.register, Temp.temp) Hashtbl.t
    type data = {interference : Liveness.igraph;
                 initial: allocation;
                 spillCost: Graph.node -> int;
                 registers: Frame.register list}
    val color : data -> allocation * Temp.temp list
end

module Make : T
