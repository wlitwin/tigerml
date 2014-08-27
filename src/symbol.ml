type symbol = string * int

let nextsym = ref 0
let sizeHint = 128
let hashtable : (string, int) Hashtbl.t =
    Hashtbl.create sizeHint

let symbol name =
   try 
       let i = Hashtbl.find hashtable name in
       (name, i)
    with _ -> 
        let i = !nextsym in
        nextsym := i + 1;
        Hashtbl.add hashtable name i;
        (name, i)
;;

let name (s, n) = 
    s
;;

type 'a table = (symbol, 'a) Hashtbl.t

let empty () = 
    let tbl : (symbol, 'a) Hashtbl.t = 
        Hashtbl.create sizeHint 
    in
    tbl
;;

let enter (tbl, sym, value) = 
    let tbl : 'a table = Hashtbl.copy tbl in
    Hashtbl.add tbl sym value;
    tbl
;;

let look (tbl, sym) = 
    try
        Some (Hashtbl.find tbl sym)
    with _ -> 
        None
;;

let print pval tbl =
    print_endline ("Size: " ^ (string_of_int (Hashtbl.length tbl)));
    Hashtbl.iter (fun k v -> 
        print_string "VAL: ";
        print_string (name k); print_string " "; 
        print_string (pval v); print_endline "")
        tbl
    ;
    flush stdout
;;
