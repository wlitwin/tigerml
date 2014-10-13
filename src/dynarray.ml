type 'a t = { len: int; mutable storage: 'a array }

exception Out_of_bounds

let new_size len =
    let l = float_of_int len in
    let l = l *. 1.5 in
    int_of_float l
;;

let make len value =
    { len; storage = Array.make len value }
;;

let copy arr =
    { len = arr.len; storage = Array.copy arr.storage }
;;

let length arr =
    arr.len
;;

let set arr idx v =
    if idx >= arr.len then (
        (* Resize the array *)
        let narr = Array.make (new_size arr.len) v in
        Array.blit arr.storage 0 narr 0 arr.len;
        arr.storage <- narr
    ) else 
        arr.storage.(idx) <- v
;;

let get arr idx =
    if idx >= arr.len then
        raise Out_of_bounds
    else 
        arr.storage.(idx)
;;
