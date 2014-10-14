type 'a t = { mutable storage: 'a array; mutable len : int; default_value: 'a }

exception Out_of_bounds

let new_size len =
    let l = float_of_int (len + 2) in
    let l = l *. 1.5 in
    int_of_float l
;;

let make len value =
    { len; storage = Array.make len value; default_value = value }
;;

let copy arr =
    { len = arr.len; storage = Array.copy arr.storage; default_value = arr.default_value}
;;

let length arr =
    arr.len
;;

let check_size arr idx =
    if idx >= (Array.length arr.storage) then (
        (*
        print_endline ("LENGTH: " ^ (string_of_int (Array.length arr.storage)));
        print_endline ("IDX: " ^ (string_of_int idx));
        *)
        let narr = Array.make (new_size idx) arr.default_value in
        Array.blit arr.storage 0 narr 0 arr.len;
        arr.storage <- narr;
        arr.len <- idx+1;
    ) else if (idx >= arr.len) then (
        arr.len <- idx+1;
    )
;;

let set arr idx v =
    (*print_endline "SET";*)
    check_size arr idx;
    arr.storage.(idx) <- v;
    (*print_endline "ESET";*)
;;

let get arr idx =
    (*
    print_endline "GET";
    print_endline ("OLD SIZE: " ^ (string_of_int (Array.length arr.storage)));
    *)
    check_size arr idx;
    (*print_endline ("nEW SIZE: " ^ (string_of_int (Array.length arr.storage)));*)
    let res = arr.storage.(idx) in
    (*print_endline "EGET";*)
    res
    (*
    if idx >= arr.len then begin
        print_endline ("INDEX: " ^ (string_of_int idx));
        print_endline ("LEN: " ^ (string_of_int arr.len));
        raise Out_of_bounds
    end else 
        arr.storage.(idx)
        *)
;;
