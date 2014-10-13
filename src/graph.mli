module type Graph =
sig
type graph
type node

val nodes: graph -> node list
val succ: node -> node list
val pred: node -> node list
val adj: node -> node list (* succ + pred *)
val eq: node * node -> bool

val numNodes: graph -> int

val newGraph: unit -> graph
val newNode: graph -> node
val copy: graph -> graph
exception GraphEdge

type node_edge = {from: node; to_: node}

val mk_edge: node_edge -> unit
val rm_edge: node_edge -> unit
val rm_node: node -> unit

module ITable : (Table.ITable with type key := node)

val nodename: node -> string (* For debugging only *)
end

module Graph : Graph
