module type Graph =
sig
type graph
type node

val nodes: graph -> node list
val succ: node -> node list
val pred: node -> node list
val adj: node -> node list (* succ + pred *)
val eq: node -> node -> bool

val numNodes: graph -> int

val newGraph: unit -> graph
val newNode: graph -> node
val copy: graph -> graph
exception GraphEdge

type node_edge = {from: node; to_: node}

val mk_edge: node_edge -> unit
val rm_edge: node_edge -> unit
val rm_node: node -> unit

val show : graph -> unit
val show_node : node -> unit

module ITable : (Table.ITable with type key := node)

val nodename: node -> string (* For debugging only *)
end 

module Graph : Graph = struct
module D = Dynarray

type node' = int

module SI = Set.Make(struct type t = int let compare = compare end)

type node_rec = {succ: SI.t; pred: SI.t}
type noderep = NODE of node_rec

let emptyNode = NODE {succ=SI.empty; pred=SI.empty}
let bogusNode = NODE {succ=SI.singleton ~-1; pred=SI.singleton ~-1}

type graph = noderep D.t
type node = graph * node'
type node_edge = {from: node; to_: node}

let isBogus (g, i : node) = 
    (D.get g i) == bogusNode
    (*
    let (bs, bp) = match bogusNode with NODE {succ=s; pred=p} -> (s, p) in
    match D.get g i with
    | NODE {succ=s; pred=p} -> 
            (s = bs) && (p = bp)
    | _ -> false
    *)
;;

let copy g =
    D.copy g
;;

let eq (_, a) (_, b) = a = b

module ITable =
    Table.MakeITable (struct 
        type key = node 
        (*let equal (g1, n1) (g2, n2) = 
            (g1 == g2) && (n1 = n2)
    *)
        let equal : key -> key -> bool = eq
        let hash (_, n) = Hashtbl.hash n
    end)

let augment (g: graph) (n: node') : node = (g, n)

let newGraph () = D.make 0 bogusNode

let nodes g =
    let lst = ref [] in
    for i=0 to D.length g - 1 do
        if not (isBogus (g, i)) then
            lst := (g, i) :: !lst
    done;
    !lst
;;

let numNodes g = List.length (nodes g)

let succ (g, i) =
    let NODE {succ=s; pred} = D.get g i in
    List.map (augment g) (SI.elements s)
;;

let pred (g, i) =
    let NODE {succ; pred=p} = D.get g i in
    List.map (augment g) (SI.elements p)
;;

let adj (g, i) = 
    let NODE {succ=s; pred} = D.get g i in
    let NODE {succ; pred=p} = D.get g i in
    List.map (augment g) (SI.elements (SI.union s p))
;;

let newNode g = 
    (* Binary search for unused node *)
    (*
    let rec look (hi, lo) =
        (* i < lo indicates i is in use
         * i >= hi indicates i not in use *)
        if lo = hi then (
            D.set g lo emptyNode;
            (g, lo)
        ) else (
            let m = (lo + hi) / 2 in
            if isBogus (D.get g m) then look (lo, m) else look (m+1, hi)
        )
    in
    look (0, 1 + D.length g)
    *)
    let idx = D.length g in
    D.set g idx emptyNode;
    (g, idx)
;;

exception GraphEdge
let check (g, g') = ()

let rec delete (elem, set) = 
    if not (SI.mem elem set) then
        raise GraphEdge
    else
        SI.remove elem set
;;

let diddle_edge change (node : node_edge) : unit =
    let {from = (g, i : graph * int); to_=(g', j : graph * int)} = node in
    check (g, g');
    let NODE {succ=si; pred=pi} = D.get g i in
    D.set g i (NODE {succ=change (j, si); pred=pi});
    let NODE {succ=sj; pred=pj} = D.get g j in
    D.set g j (NODE {succ=sj; pred=change (i, pj)})
;;

let mk_edge : node_edge -> unit = diddle_edge (fun (a, b) -> SI.add a b)
let rm_edge : node_edge -> unit = diddle_edge delete

let print_node node =
    match node with
    | NODE {succ; pred} ->
            print_string "PRED: ";
            SI.iter (fun p ->
                print_string (string_of_int p); 
                print_string " ";
            ) pred;
            print_string " SUCC: ";
            SI.iter (fun s ->
                print_string (string_of_int s); 
                print_string " ";
            ) succ;
            print_endline "";
;;

let show_node (g, node : node) =
    print_string ("NODE: " ^ (string_of_int node) ^ " ");
    print_node (D.get g node)
;;

let show (g : graph) : unit =
    for i=0 to (D.length g)-1 do
        if not (isBogus (g, i)) then begin
            print_string ("NODE : " ^ (string_of_int i) ^ " ");
            print_node (D.get g i);
        end
    done
;;

let rm_node node : unit =
    let (n, i) = node in
    let succs = succ node in
    let preds = pred node in
    List.iter (fun neighbor ->
        let (nn, ni) = neighbor in
        if not (i = ni) then
            rm_edge {from = neighbor; to_ = node}
    ) preds;
    List.iter (fun neighbor ->
        let (nn, ni) = neighbor in
        if not (i = ni) then
            rm_edge {from = node; to_ = neighbor}
    ) succs;
    let (g, i) = node in
(*    print_string "BOGUS? "; print_string (string_of_bool (isBogus (g, i))); print_endline"";*)
    D.set g i bogusNode;
    (*print_endline "NOW BOGUS? "; print_string (string_of_bool (isBogus (g, i))); print_endline ""; *)
;;

let nodename (g, i : node) = "n" ^ (string_of_int i)
end
