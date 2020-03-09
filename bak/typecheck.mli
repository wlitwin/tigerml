type pos = int * int
type tyinfo = { pos: pos; ty: Types.ty }

type ast_in = (pos Ast.exp, pos) Ast.ast_value
type ast_out = (tyinfo Ast.exp, tyinfo) Ast.ast_value

val typecheck : ast_in -> ast_out
