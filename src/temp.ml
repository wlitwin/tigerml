type temp = int
type label = Symbol.symbol

module ITable : (Table.ITable with type key := temp) =
    Table.MakeITable (struct type key = temp let equal = (=) let hash = Hashtbl.hash end)

let temps = ref 0

let newtemp () = 
    let t = !temps in
    temps := t+1;
    t
;;

let makestring t = 
    "t" ^ (string_of_int t)
;;

let labs = ref 0
let newlabel () =
    let sym = 
        Symbol.symbol ("L" ^ (string_of_int !labs)) 
    in
    labs := !labs + 1;
    sym
;;

let namedlabel = Symbol.symbol
