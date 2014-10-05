type temp = int
val newtemp : unit -> temp
val makestring : temp -> string
type label = Symbol.symbol
val newlabel : unit -> label
val namedlabel : string -> label 

module ITable : (Table.ITable with type key := temp)
