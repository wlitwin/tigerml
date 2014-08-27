type label = Temp.label
type size = int

type stm = SEQ of stm * stm
         | LABEL of label
         | JUMP of exp * label list
         | CJUMP of relop * exp * exp * label * label
         | MOVE of exp * exp
         | EXP of exp
and exp = BINOP of binop * exp * exp
        | MEM of exp
        | TEMP of Temp.temp
        | ESEQ of stm * exp
        | NAME of label
        | CONST of int
        | CALL of exp * exp list
and binop = PLUS | MINUS | MUL | DIV 
          | AND  | OR | LSHIFT | RSHIFT
          | ARSHIFT | XOR
and relop = EQ | NE | LT | GT | LE | GE
          | ULT | ULE | UGT | UGE

let notRel (op : relop) : relop =
    match op with
    | EQ -> NE
    | NE -> EQ
    | LT -> GE
    | LE -> GT
    | GT -> LE
    | GE -> LT
    | ULT -> UGE
    | ULE -> UGT
    | UGT -> ULE
    | UGE -> ULT
;;

let commute (op : relop) : relop =
    EQ
;;

let seq (lst : stm list) : stm =
    let rec build_seq = function
        | [] -> failwith "Can't build sequence from nothing"
        | [item] -> item
        | i1 :: i2 :: [] -> SEQ (i1, i2)
        | hd :: tl ->
                let bot = build_seq tl in
                SEQ (hd, bot)
    in
    build_seq lst
;;
