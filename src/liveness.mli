module Graph = Graph.Graph

type igraph = { graph: Graph.graph;
                tnode: Graph.node Temp.ITable.table;
                gtemp: Temp.temp Flowgraph.FGraph.ITable.table;
                moves: (Graph.node * Graph.node) list }

module ST : (Set.S with type elt = Temp.temp)

val interferenceGraph : Flowgraph.flowgraph -> igraph * ST.t Flowgraph.FGraph.ITable.table

val show : out_channel * igraph -> unit
