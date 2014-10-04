module FGraph = Graph.Graph

type flowgraph = {control: FGraph.graph;
                  def: Temp.temp list FGraph.Table.table;
                  use: Temp.temp list FGraph.Table.table;
                  ismove: bool FGraph.Table.table}

(* Note: Any "use" within the block is assumed to be BEFORE a "def"
 *       of the same variable. If there is a def(x) followed by use(x)
 *       in the same block, do not mention the use in this data structure,
 *       mention only the def.
 *
 *       More generally:
 *          If there are any nonzero number of defs, mention def(x).
 *          If there are any nonzero number of uses BEFORE THE FIRST DEF,
 *              mention use(x)
 *
 *       For any node in the graph,
 *          Graph.table.look (def, node) = Some (def-list)
 *          Grpah.table.look (use, node) = Some (use-list)
 *)
