type pos = int * int
and symbol = Symbol.symbol

type 'a var = SimpleVar of symbol * 'a
            | FieldVar of 'a var * symbol * 'a
            | SubscriptVar of 'a var * 'a exp * 'a
and 'a exp = VarExp of 'a var * 'a
           | NilExp of unit * 'a
           | IntExp of int * 'a
           | StringExp of string * 'a
                        (* func    params *)
           | CallExp of (symbol * 'a exp list) * 'a
                      (* left    op     right *)
           | OpExp of ('a exp * oper * 'a exp) * 'a
                           (* fields               type *)
           | RecordExp of ((symbol * 'a exp) list * symbol) * 'a  
           | SeqExp of 'a exp list * 'a
           | AssignExp of ('a var * 'a exp) * 'a
           | IfExp of ('a exp * 'a exp * 'a exp option) * 'a
           | WhileExp of ('a exp * 'a exp) * 'a
                       (* var    escape      lo       hi       body *)
           | ForExp of (symbol * bool ref * 'a exp * 'a exp * 'a exp) * 'a
           | BreakExp of unit * 'a
                       (* decs      body*)
           | LetExp of ('a dec list * 'a exp list) * 'a
                         (* type    size      init *)
           | ArrayExp of (symbol * 'a exp * 'a exp) * 'a

and 'a dec = FunctionDec of 'a fundec list * 'a
                       (* name    escape    type            init *)
           | VarDec of (symbol * bool ref * (symbol * 'a) option * 'a exp) * 'a
           | TypeDec of ((symbol * 'a ty * 'a) list) * 'a

               (* name   escape     type *)
and 'a field = (symbol * bool ref * symbol) * 'a

             (* name     params         result                body *)
and 'a fundec = (symbol * 'a field list * (symbol * 'a) option * 'a exp) * 'a

and 'a ty = NameTy of symbol * 'a
          | RecordTy of 'a field list * 'a
          | ArrayTy of symbol * 'a

and oper = PlusOp
         | MinusOp
         | TimesOp
         | DivideOp
         | EqOp
         | NeqOp
         | LtOp
         | LeOp
         | GtOp
         | GeOp

let getVal (exp : 'a exp) =
    match exp with
    | VarExp (_, p) -> p
    | NilExp (_, p) -> p
    | IntExp (_, p) -> p
    | OpExp (_, p) -> p
    | StringExp (_, p) -> p
    | CallExp (_, p) -> p
    | RecordExp (_, p) -> p
    | SeqExp (_, p) -> p
    | AssignExp (_, p) -> p
    | IfExp (_, p) -> p
    | WhileExp (_, p) -> p
    | ForExp (_, p) -> p
    | BreakExp (_, p) -> p
    | LetExp (_, p) -> p
    | ArrayExp (_, p) -> p
;;

