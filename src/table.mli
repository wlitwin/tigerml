module type Table =
sig
    type key
    type 'a table

    val empty : unit -> 'a table
    val enter : 'a table * key * 'a -> 'a table
    val look  : 'a table * key -> 'a option
    val print : (key -> string) -> ('a -> string) -> 'a table -> unit
end

module type ITable =
sig
    type key
    type 'a table

    val empty : unit -> 'a table
    val clone : 'a table -> 'a table
    val union_exn : 'a table -> 'a table -> 'a table
    val enter : 'a table * key * 'a -> unit
    val look  : 'a table * key -> 'a option
    val print : (key -> string) -> ('a -> string) -> 'a table -> unit
end

module type KeyValue =
    sig
        type key
    end

module Make (KV : KeyValue) : Table
module MakeITable (KV : KeyValue) : (ITable with type key := KV.key)
