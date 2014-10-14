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
    val look_exn : 'a table -> key -> 'a
    val iter : (key -> 'a -> unit) -> 'a table -> unit
    val print : (key -> string) -> ('a -> string) -> 'a table -> unit

    exception KeyNotFound
end

module type KeyValue =
sig
    type key
    val equal : key -> key -> bool
    val hash : key -> int
end

module MakeITable (KV : KeyValue) : (ITable with type key := KV.key) =
struct
    type key = KV.key

    let sizeHint = 10

    module Hashtbl = Hashtbl.Make(struct 
        type t = key 
        let equal = KV.equal 
        let hash = KV.hash
    end)

    type 'a table = 'a Hashtbl.t

    let clone table = Hashtbl.copy table

    let empty () : 'a table = 
        Hashtbl.create sizeHint
    ;;

    let enter (table, key, value) =
        Hashtbl.replace table key value;
    ;;

    let look (table, key) : 'a option =
        try Some (Hashtbl.find table key)
        with _ -> None
    ;;

    exception KeyNotFound
    let look_exn table key : 'a =
        try Hashtbl.find table key
        with _ -> raise KeyNotFound
    ;;

    let union_exn tbl1 tbl2 =
        let out = clone tbl1 in
        Hashtbl.iter (fun k v -> 
            match look (tbl1, k) with
            | Some v1 -> if not (v1 = v) then 
                failwith "Key exists with different value!"
            | None -> Hashtbl.replace out k v
        ) tbl2;
        out
    ;;

    let iter f (table : 'a table) =
        Hashtbl.iter f table
    ;;

    let print (strKey : key -> string) (strVal : 'a -> string) (tbl : 'a table) : unit =
        print_endline ("Size: " ^ (string_of_int (Hashtbl.length tbl)));
        Hashtbl.iter (fun k v ->
            print_string "Entry: ";
            print_string (strKey k); print_string " ";
            print_string (strVal v); print_endline "")
            tbl
    ;;
end

module Make (KV : KeyValue) =
struct
    type key = KV.key

    let sizeHint = 10

    type 'a table = (key, 'a) Hashtbl.t

    let empty () : 'a table = 
        Hashtbl.create sizeHint
    ;;

    let enter (table, key, value) : 'a table =
        let copy = Hashtbl.copy table in
        Hashtbl.add copy key value;
        copy
    ;;

    let look (table, key) : 'a option =
        try Some (Hashtbl.find table key)
        with _ -> None
    ;;

    let print (strKey : key -> string) (strVal : 'a -> string) (tbl : 'a table) : unit =
        print_endline ("Size: " ^ (string_of_int (Hashtbl.length tbl)));
        Hashtbl.iter (fun k v ->
            print_string "Entry: ";
            print_string (strKey k); print_string " ";
            print_string (strVal v); print_endline "")
            tbl
    ;;
end
