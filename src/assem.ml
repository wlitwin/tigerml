type reg = string
type temp = Temp.temp
type label = Temp.label

type rec_oper = { assem: string; 
                  dst: temp list;
                  src: temp list;
                  jump: label list option }

type rec_label = { assem: string;
                   lab: Temp.label }

type rec_move = { assem: string;
                  dst: temp;
                  src: temp }

type instr = OPER of rec_oper
           | LABEL of rec_label
           | MOVE of rec_move

let explode str = 
    let rec expl i acc =
        if i >= 0 then expl (i-1) (str.[i] :: acc)
        else acc
    in
    expl (String.length str - 1) []
;;

let implode lst =
    let len = List.length lst in
    let str = Bytes.create len in
    List.iteri (fun idx chr -> Bytes.set str idx  chr) lst;
    str
;; 

let ord = Char.code

let print_instr = function
    | OPER {assem; dst; src; jump} ->
          print_string assem;
          print_string " dst: ";
          List.iter (fun dst ->
              print_string (Temp.makestring dst);
              print_string ", "
          ) dst;
          print_string " src: ";
          List.iter (fun src ->
              print_string (Temp.makestring src);
              print_string ", "
          ) src;
          (match jump with
          | Some jumps ->
                  print_string " jump: ";
                  List.iter (fun jump -> 
                    print_string (Symbol.name jump);
                    print_string ", ";
                  ) jumps;
          | None -> ()
          )
          ;
          print_endline ""
    | LABEL {assem; lab} -> 
            print_string (assem ^ " ");
            print_endline ("LABEL: " ^ (Symbol.name lab))
    | MOVE {assem; src; dst;} ->
            print_endline (assem ^ " src: " ^ (Temp.makestring src) ^ 
                                   " dst: " ^ (Temp.makestring dst))
          

let format saytemp instr : bytes =
    let speak (assem, dst, src, jump) =
        let saylab = Symbol.name in
        let rec f = function
            | '`' :: 's' :: i :: rest ->
                    (explode (saytemp (List.nth src (ord i - ord '0')))) @ (f rest)
            | '`' :: 'd' :: i :: rest ->
                    (explode (saytemp (List.nth dst (ord i - ord '0')))) @ (f rest)
            | '`' :: 'j' :: i :: rest ->
                    (explode (saylab (List.nth jump (ord i - ord '0')))) @ (f rest)
            | '`' :: '`' :: rest -> '`' :: (f rest)
            | '`' :: _ :: rest -> ErrorMsg.impossible "bad Assem format"
            | c :: rest -> c :: (f rest)
            | [] -> []
        in
        implode (f (explode assem))
    in
    match instr with 
    | OPER {assem; dst; src; jump=None} -> speak (assem, dst, src, [])
    | OPER {assem; dst; src; jump=Some j} -> speak (assem, dst, src, j)
    | LABEL {assem; lab} -> Bytes.of_string assem
    | MOVE {assem; dst; src} -> speak (assem, [dst], [src], [])
