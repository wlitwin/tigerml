type op = PlusOp
        | MinusOp
        | TimesOp
        | DivideOp
        | EqOp
        | NeqOp
        | LtOp
        | LeOp
        | GtOp
        | GeOp

type stm = Def of string * exp
and exp = Const of int
        | String of string
        | NilExp
