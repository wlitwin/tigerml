(*type ('a, 'b) pair = { data: 'a; ex: 'b }

type pos = int * int

type tyrec = { pos: pos; ty: Types.ty; }

type tree_exp = tyrec Absyn.exp
type tree_var = tyrec Absyn.var
type tree_dec = tyrec Absyn.dec
*)
