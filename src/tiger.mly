%{
    module A = Absyn
    module S = Symbol
    type span = int * int
    let dummy_pos = (-1, -1)

    type 'a lval = Subscript of 'a A.exp * A.pos
                 | Field of S.symbol * A.pos

%}

%token EOF
%token <string * (int * int) > ID 
%token <int * (int * int)> INT 
%token <string * (int * int)> STRING 
%token <int * int> COMMA 
%token <int * int> COLON 
%token <int * int> SEMICOLON 
%token <int * int> LPAREN 
%token <int * int> RPAREN 
%token <int * int> LBRACK 
%token <int * int> RBRACK
%token <int * int> LBRACE
%token <int * int> RBRACE
%token <int * int> DOT
%token <int * int> PLUS
%token <int * int> MINUS
%token <int * int> TIMES
%token <int * int> DIVIDE
%token <int * int> EQ
%token <int * int> NEQ
%token <int * int> LT
%token <int * int> LE
%token <int * int> GT
%token <int * int> GE
%token <int * int> AND
%token <int * int> OR
%token <int * int> ASSIGN
%token <int * int> ARRAY
%token <int * int> IF
%token <int * int> THEN
%token <int * int> ELSE
%token <int * int> WHILE
%token <int * int> FOR
%token <int * int> TO
%token <int * int> DO
%token <int * int> LET
%token <int * int> IN
%token <int * int> END
%token <int * int> OF
%token <int * int> BREAK
%token <int * int> NIL
%token <int * int> FUNCTION
%token <int * int> VAR
%token <int * int> TYPE

%start <Absyn.pos Absyn.exp> program

%nonassoc LOWEST
%nonassoc DO THEN OF ASSIGN FUNCTION TYPE
%right ELSE
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%%

program: exp EOF { $1 }

exp: NIL { A.NilExp ((), $1) }
   | INT { let (i, p) = $1 in A.IntExp (i, p) }
   | STRING { let (s, p) = $1 in A.StringExp (s, p) }
   | LPAREN RPAREN { A.SeqExp ([], $1) }
   | LPAREN exp RPAREN { A.SeqExp ([$2], $1) }
   | assign_exp { $1 }
   | cmp_exp { $1 }
   | arith_exp { $1 }
   | bool_exp { $1 }
   | control_exp { $1 }
   | seq_exp { $1 }
   | let_exp { $1 }
   | array_create { $1 }
   | record_create { $1 }
   | funcall { $1 }
   | lvalue { let (v, p) = $1 in A.VarExp (v, p) }

assign_exp: lvalue ASSIGN exp 
         { 
            let (v, p) = $1 in
            A.AssignExp ((v, $3), p)
         }

funcall: _id = id; LPAREN; lst = separated_list(COMMA, exp); RPAREN 
         {  
             let (s, p) = _id in
             A.CallExp ((s, lst), p)
         }

(* Resolve conflict with array_create *)
lvalue: id lvalue_rest { let (s, p) = $1 in 
                         let lst = $2 in
                         let var =
                            List.fold_left 
                            (fun acc v ->
                                match v with
                                | Field (s, p) -> A.FieldVar (acc, s, p)
                                | Subscript (e, p) -> A.SubscriptVar (acc, e, p)
                            )
                            (A.SimpleVar (s, p))
                            lst
                         in
                         (var, p)
                       }
lvalue_rest: DOT id lvalue_rest { let (s, p) = $2 in (Field (s, p)) :: $3 }
           | LBRACK exp RBRACK lvalue_rest { let p = A.getVal $2 in (Subscript ($2, p)) :: $4 }
           | { [] }

array_create: id LBRACK exp RBRACK OF exp 
              {  
                let (s, p) = $1 in
                A.ArrayExp ((s, $3, $6), p)
              }

record_create: _id = id; LBRACE; lst = separated_list(COMMA, record_field); RBRACE 
              {  
                let (s, p) = _id in
                A.RecordExp ((lst, s), p)
              }
record_field: id EQ exp 
              {  
                let (s, _) = $1 in
                (s, $3) 
              }
                

let_exp: pos = LET; _decs = decs; IN; lst = separated_list(SEMICOLON, exp); END 
        { 
            A.LetExp ((_decs, lst), pos)
        }

control_exp: IF exp THEN exp { A.IfExp (($2, $4, None), $1) }
           | IF exp THEN exp ELSE exp { A.IfExp (($2, $4, Some $6), $1) }
           | WHILE exp DO exp { A.WhileExp (($2, $4), $1) }
           | FOR id ASSIGN exp TO exp DO exp 
                { let (s, _) = $2 in 
                  A.ForExp ((s, ref true, $4, $6, $8), $1) 
                }
           | BREAK { A.BreakExp ((), $1) }

seq_exp: LPAREN seq_exp_inner RPAREN { A.SeqExp ($2, $1) }
seq_exp_inner: exp SEMICOLON exp { [$1; $3] }
             | seq_exp_inner SEMICOLON exp { $1 @ [$3] }

cmp_exp: exp EQ exp  { A.OpExp (($1, A.EqOp, $3), $2)  }
       | exp NEQ exp { A.OpExp (($1, A.NeqOp, $3), $2) }
       | exp LT exp  { A.OpExp (($1, A.LtOp, $3), $2)  }
       | exp LE exp  { A.OpExp (($1, A.LeOp, $3), $2)  }
       | exp GT exp  { A.OpExp (($1, A.GtOp, $3), $2)  }
       | exp GE exp  { A.OpExp (($1, A.GeOp, $3), $2)  }

bool_exp: exp AND exp 
            { let (p1, _) = A.getVal $1
              and (_, p2) = A.getVal $3 in
              A.IfExp (($1, $3, Some (A.IntExp (0, dummy_pos))), (p1, p2))
            }
        | exp OR exp 
            { let (p1, _) = A.getVal $1
              and (_, p2) = A.getVal $3 in
              A.IfExp (($1, A.IntExp (1, dummy_pos), Some $3), (p1, p2))
            }

arith_exp: exp PLUS exp { A.OpExp (($1, A.PlusOp, $3), $2) }
         | exp MINUS exp { A.OpExp (($1, A.MinusOp, $3), $2) }
         | exp TIMES exp { A.OpExp (($1, A.TimesOp, $3), $2) }
         | exp DIVIDE exp { A.OpExp (($1, A.DivideOp, $3), $2) }
         | MINUS exp %prec UMINUS 
            { 
                let (p1, _) = $1  
                and (_, p2) = A.getVal $2 in
                A.OpExp ((A.IntExp (0, dummy_pos), A.MinusOp, $2), (p1, p2))
            }

(* Identifiers *)
id: ID { let (n, p) = $1 in (S.symbol n, p) }

(* Declarations *)
decs: { [] }
    | dec decs { $1 :: $2 }

dec: lst = tydecs %prec LOWEST { A.TypeDec (lst, dummy_pos) }
   | vardec { $1 }
   | lst = fundecs %prec LOWEST { A.FunctionDec (lst, dummy_pos) }

(* Type Declarations *)
tydecs: tydec { [$1] }
      | tydecs tydec { $1 @ [$2] }
tydec: TYPE type_id EQ ty { let (s, p) = $2 in (s, $4, p) }

ty: type_id { let (s, p) = $1 in A.NameTy (s, p) }
  | LBRACE tyfields RBRACE { A.RecordTy ($2, $1) }
  | ARRAY OF type_id { let (s, p) = $3 in A.ArrayTy (s, p) }

type_id: id { $1 }

tyfields: lst = separated_list(COMMA, tyfield_inner) 
            {
                lst         
            }
tyfield_inner: id COLON type_id 
            { 
                let (s1, p1) = $1 
                and (s2, _) = $3 in
                ((s1, ref true, s2), p1)
            }

(* Variable Declarations *)
vardec: VAR id ASSIGN exp 
            {  
                let (s, p) = $2 in
                A.VarDec ((s, ref true, None, $4), p)
            }
      | VAR id COLON type_id ASSIGN exp 
            {
                let (s, p) = $2 in
                A.VarDec ((s, ref true, Some $4, $6), p)
            }

(* Function Declarations *)
fundecs: fundec { [$1] }
       | fundecs fundec { $1 @ [$2] }
fundec: FUNCTION id LPAREN tyfields RPAREN EQ exp 
          {  
                let (s, p) = $2 in
                ((s, $4, None, $7), p)
          } 
      | FUNCTION id LPAREN tyfields RPAREN COLON type_id EQ exp
          {
                let (s, p) = $2 in
                ((s, $4, Some $7, $9), p)
          }

