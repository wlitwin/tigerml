let anyErrors = ref false
let fileName = ref "" 
let lineNum = ref 1
let linePos = ref [1]
let sourceStream = ref (Stream.of_channel stdin)

let reset () = 
    anyErrors := false;
    fileName := "";
    lineNum := 1;
    linePos := [1];
    sourceStream := Stream.of_channel stdin
;;

exception Error

let error (pos, pos2) (msg : string) =
    let rec look = function
        | (a :: rest, n) -> 
            if a < pos then (
                print_string ":";
                print_int n;
                print_string ".";
                print_int (pos - a)
            ) else (
                look (rest, n - 1)
            )
        | _ -> print_string "0.0"
    in
    anyErrors := true;
    print_string !fileName;
    look (!linePos, !lineNum);
    print_string ": ";
    print_string msg;
    print_endline "";
;;

let impossible msg =
    print_string "Error: Compiler bug: ";
    print_string msg;
    print_endline "";
    flush stdout; 
    raise Error
;;
