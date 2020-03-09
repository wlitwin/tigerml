
module LG = Liveness.Graph
module LGI = LG.ITable
module TI = Temp.ITable

type color = int

val color: Liveness.igraph -> (LG.graph * color TI.table)
