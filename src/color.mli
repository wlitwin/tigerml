module type T =
    functor (Frame : Frame.Frame) ->
sig
    module Graph = Graph.Graph

    type allocation = Frame.register Temp.ITable.table
    type data = {interference : Liveness.igraph;
                 initial: allocation;
                 spillCost: Graph.node -> int;
                 registers: Frame.register list}
    val color : data -> allocation * Temp.temp list
end

module Make : T
