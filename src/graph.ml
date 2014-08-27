type node' = int
type temp = Temp.temp

type node_rec = {succ: node' list; pred: node' list}

type noderep = NODE of node_rec

let emptyNode = NODE {succ=[]; pred=[]}
let bogusNode = NODE {succ=[-1]; pred=[]}

let isBogus = function
    | NODE {succ = (-1 :: _); pred} -> true
    | _ -> false
;;


