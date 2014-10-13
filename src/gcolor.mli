
module LG = Liveness.Graph
module LGI = LG.ITable

type color = int

val color: Liveness.igraph -> (LG.graph * color LGI.table)
