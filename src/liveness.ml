type igraph = { graph: Graph.graph;
                tnode: Temp.temp -> Graph.node; 
                gtemp: Graph.node -> Temp.temp;
                moves: (Graph.node * Graph.node) list }

type liveSet = unit Temp.Table.table * temp list
type liveMap = liveSet Flow.Graph.Table.table

let interferenceGraph (fgraph : Flowgraph.flowgraph) : igraph * (Flow.Graph.node -> Temp.temp list) =
    
;;

val show : out_channel * igraph -> unit
