
module D = Dynarray

type node' = int
type temp = Temp.temp
type node_rec = {succ: node' list; pred: node' list}
type noderep = NODE of node_rec
type graph = noderep D.t
type node = graph * node'
type node_edge = {from: node; to_: node}

type 'a table = (int, 'a) Hashtbl.t

let emptyNode = NODE {succ=[]; pred=[]}
let bogusNode = NODE {succ=[-1]; pred=[]}

let isBogus = function
    | NODE {succ = (-1 :: _); pred} -> true
    | _ -> false
;;

let eq ((_, a), (_, b)) = a = b

let augment (g: graph) (n: node') : node = (g, n)

let newGraph () = D.make 0 bogusNode

let nodes g =
    let rec f i = 
        if isBogus (D.get g i) then []
        else (g, i) :: f (i+1)
    in
    f 0
;;

let succ (g, i) =
    let NODE {succ=s; pred} = D.get g i in
    List.map (augment g) s
;;

let pred (g, i) =
    let NODE {succ; pred=p} = D.get g i in
    List.map (augment g) p
;;

let adj gi = 
    pred gi @ succ gi
;;

let newNode g = 
    (* Binary search for unused node *)
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
;;

exception GraphEdge
let check (g, g') = ()

let rec delete = function
    | (i, j :: rest) -> if i = j then rest else j :: delete (i, rest)
    | (_, []) -> raise GraphEdge
;;

let diddle_edge change (node : node_edge) : unit =
    let {from = (g, i : graph * int); to_=(g', j : graph * int)} = node in
    check (g, g');
    let NODE {succ=si; pred=pi} = D.get g i in
    D.set g i (NODE {succ=change (j, si); pred=pi});
    let NODE {succ=sj; pred=pj} = D.get g j in
    D.set g j (NODE {succ=sj; pred=change (i, pj)})
;;

let mk_edge : node_edge -> unit = diddle_edge (fun (a, b) -> a :: b)
let rm_edge : node_edge -> unit = diddle_edge delete

let nodename (g, i : node) = "n" ^ (string_of_int i)
