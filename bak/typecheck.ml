module A = Ast
module S = Symbol

type enventry = VarEntry of Types.ty
              | FunEntry of string * Types.ty list * Types.ty

type venv = enventry Symbol.table
type tenv = Types.ty Symbol.table

type pos = int * int
type tyinfo = { pos: pos; ty: Types.ty }

type ast_in = (pos Ast.exp, pos) Ast.ast_value
type ast_out = (tyinfo Ast.exp, tyinfo) Ast.ast_value

type var_in = (pos Ast.var, pos) Ast.ast_value
type var_out = (tyinfo Ast.var, tyinfo) Ast.ast_value

type dec_in = pos Ast.dec
type dec_out = tyinfo Ast.dec

module SS = Set.Make(String)

exception Impossible
exception Error

let error (ast_val : ('a, pos) Ast.ast_value) msg =
    ErrorMsg.error ast_val.Ast.info msg;
    raise Error
;;

(* Internal Compiler Error *)
let ice msg =
    print_endline msg;
    flush stdout;
    raise Impossible
;;

let rec checkType ty ty_expect msg pos : unit =
    let open Types in
    match (ty, ty_expect) with
    (* Simple checks *)
    | (NIL, NIL) | (INT, INT)
    | (NIL, RECORD _) | (RECORD _, NIL)
    | (UNIT, UNIT) | (STRING, STRING) -> () 
    (* Records and arrays have unique types *)
    | (RECORD (_, u1), RECORD (_, u2)) -> if u1 <> u2 then error pos msg
    | (ARRAY (_, u1), ARRAY (_, u2)) -> if u1 <> u2 then error pos msg
    | (NAME (_, r1), ty) -> (match !r1 with 
            | Some ty -> checkType ty ty_expect msg pos
            | None -> error pos msg)
    | (ty, NAME (_, r1)) -> (match !r1 with 
            | Some ty_expect -> checkType ty ty_expect msg pos
            | None -> error pos msg)
    | _ -> error pos msg
;;

let checkInt ty (ast_val : ('a, pos) Ast.ast_value) =
    checkType ty Types.INT "Expected integer expression" ast_val
;;

let mkVal (ain : ('b, pos) Ast.ast_value) (nval : 'a) (ty : Types.ty) : ('a, tyinfo) Ast.ast_value =
    Ast.{nval; info={pos=ain.info; ty}}
;;

let rec actualTy : Types.ty -> Types.ty = function
    | Types.NAME(_, op) ->
            begin match !op with
            | Some ty -> actualTy ty
            | None -> ice "Invalid name object"
            end
    | Types.ARRAY(ty, uniq) -> Types.ARRAY(actualTy ty, uniq)
    | ty -> ty
;;

let lookupTy (tbl, ty, pos, msg) =
    match S.look(tbl, ty) with
    | Some ty -> ty
    | None -> error pos msg
;;

let transTy (tenv, ty, pos : tenv * pos Ast.ty * ('a Ast.typdec_rec, 'b) Ast.ast_value) : (tyinfo Ast.typdec_rec, tyinfo) Ast.ast_value =
    let open Ast in
    let msg sym = "Type " ^ S.name sym ^ " does not exist in this scope" in
    match ty with
    | NameTy sym -> 
            (* TODO - rethink the type dec AST, this is very messy *)
            let ty = lookupTy(tenv, sym, pos, msg sym) in
            let type_ = {nval=NameTy sym; info={pos=pos.info; ty}} in
            let trec = {typ_name=pos.nval.typ_name; type_} in
            {nval=trec; info={pos=pos.info; ty}}
    | RecordTy fields ->
            let rec transFields (tylst, flst) = function
                | [] -> (List.rev tylst, List.rev flst)
                | frec :: tl ->
                        let tyname = frec.nval.ptype in
                        let ty = lookupTy(tenv, tyname, pos, (msg tyname)) in
                        let field = {nval=frec.nval; info={pos=frec.info; ty}} in
                        transFields ((tyname, ty) :: tylst, field :: flst) tl
            in
            let (tylst, fields) = transFields ([], []) fields in
            let ty = Types.RECORD(tylst, ref ()) in
            let type_ = {nval=RecordTy fields; info={pos=pos.info; ty}} in
            let trec = {typ_name=pos.nval.typ_name; type_} in
            {nval=trec; info={pos=pos.info; ty}}
    | ArrayTy sym ->
            let ty = lookupTy(tenv, sym, pos, (msg sym)) in
            let ty = Types.ARRAY(ty, ref ()) in
            let type_ = {nval=ArrayTy sym; info={pos=pos.info; ty}} in
            let trec = {typ_name=pos.nval.typ_name; type_} in
            {nval=trec; info={pos=pos.info; ty}}

let fold_rev f s l = List.rev (List.fold_left f s l)

(* TODO - Add loop parameter *)
let rec typecheck_exp (exp, venv, tenv : ast_in * venv * tenv) : ast_out =
    let open Ast in
    let mkVal = mkVal exp in
    match exp.nval with
    | NilExp     -> mkVal NilExp Types.NIL
    | BreakExp   -> mkVal BreakExp Types.UNIT
    | IntExp num -> mkVal (IntExp num) Types.INT
    | StringExp str -> mkVal (StringExp str) Types.STRING
    | VarExp var -> 
            let var = typecheck_var(var, venv, tenv) in
            mkVal (VarExp var) var.info.ty
    | OpExp {left; op; right} ->
            let left = typecheck_exp(left, venv, tenv) in
            let right = typecheck_exp(right, venv, tenv) in
            begin match op with
            | PlusOp | MinusOp | TimesOp | DivideOp ->
                    checkType left.info.ty Types.INT
                        ("Left operand should be of integer type, got " ^ Types.str left.info.ty) exp;
                    checkType left.info.ty Types.INT
                        ("Right operand should be of integer type, got " ^ Types.str right.info.ty) exp;
            | EqOp | LtOp | LeOp | NeqOp | GtOp | GeOp ->
                    let tyleft = actualTy left.info.ty
                    and tyright = actualTy right.info.ty in
                    let open Types in
                    match (tyleft, tyright) with
                    | (STRING, STRING) | (INT, INT) | (NIL, NIL) | (NIL, RECORD _)
                    | (RECORD _, NIL) | (ARRAY _, ARRAY _) | (RECORD _, RECORD _) -> ()
                    | _ -> error exp ("Can't compare " ^ Types.str tyleft ^ " and " ^ Types.str tyright)
            end;
            mkVal (OpExp {left; op; right}) Types.INT
    | SeqExp lst ->
            let (lst, ty) = List.fold_left (fun (acc, lastty) exp -> 
                    let exp = typecheck_exp(exp, venv, tenv) in
                    (exp :: acc, exp.info.ty)
                ) ([], Types.UNIT) lst 
            in
            mkVal (SeqExp (List.rev lst)) ty
    | ForExp frec ->
            (* TODO add loopvar to the environment *)
            let loopvar = typecheck_var(frec.loopvar, venv, tenv) in
            let lo_exp = typecheck_exp(frec.lo_exp, venv, tenv) in
            checkType lo_exp.info.ty Types.INT 
                ("For loop lower bound must be of integer type, got " ^ Types.str lo_exp.info.ty)
                exp;
            let hi_exp = typecheck_exp(frec.hi_exp, venv, tenv) in
            checkType hi_exp.info.ty Types.INT
                ("For loop upper bound must be of integer type, got " ^ Types.str hi_exp.info.ty)
                exp;
            let fbody = typecheck_exp(frec.fbody, venv, tenv) in
            checkType fbody.info.ty Types.UNIT
                ("For loop body must be of unit type, got " ^ Types.str fbody.info.ty)
                exp;
            let frec = {frec with loopvar; lo_exp; hi_exp; fbody} in
            mkVal (ForExp frec) Types.UNIT
    | RecordExp rrec ->
            let tyrec = actualTy (lookupTy(tenv, rrec.record_name, exp,
                                  "Record type " ^ S.name rrec.record_name ^ " is not defined"))
            in
            let fields = match tyrec with
                | Types.RECORD(fields, _) ->
                        (* Check the fields *)
                        let rec fieldType name flist : Types.ty =
                            match flist with
                            | [] -> error exp (S.name name ^ " does not exist for this record type")
                            | (field, ty) :: tl -> if field=name then ty else fieldType name tl
                        in
                        let fields = fold_rev (fun acc (sym, fexp) -> 
                                let fexp = typecheck_exp(exp, venv, tenv) in
                                let fty = fieldType sym fields in
                                checkType fexp.info.ty fty
                                    ("Exp is not of the correct type for field " ^ 
                                     S.name sym ^ " = " ^ Types.str (actualTy fty))
                                    exp;
                                ((sym, fexp) :: acc)
                            ) [] rrec.fields
                        in
                        fields
                | _ -> error exp ("Not of record type " ^ Types.str (actualTy tyrec))
            in
            let fields = List.rev fields in
            mkVal (RecordExp {rrec with fields}) Types.UNIT
    | IfExp {test; conseq; alt} ->
            let test = typecheck_exp(test, venv, tenv)
            and conseq = typecheck_exp(conseq, venv, tenv) in
            checkType test.info.ty Types.INT
                ("If conditionals must be integer expressions, condition is of " ^
                 Types.str test.info.ty ^ " type") exp;
            (* Make sure then and else branches are the same type *)
            let tythen = actualTy conseq.info.ty in
            begin match alt with
            | Some alt -> 
                    let alt = typecheck_exp(alt, venv, tenv) in
                    let tyalt = actualTy alt.info.ty in
                    let msg = "If expressions then and else branches must have the same type" ^
                              "\n Then has type: " ^ Types.str tythen ^
                              "\n Else has type: " ^ Types.str tyalt
                    in
                    checkType tythen tyalt msg exp;
                    mkVal (IfExp {test; conseq; alt=Some alt}) tythen
            | None -> 
                    checkType tythen Types.UNIT 
                        ("If then expressions must be of unit type, got " ^ Types.str tythen) 
                        exp;
                    mkVal (IfExp {test; conseq; alt=None}) Types.UNIT
            end
    | WhileExp {wcond; wbody} ->
            let wcond = typecheck_exp(wcond, venv, tenv)
            and wbody = typecheck_exp(wbody, venv, tenv) in
            checkInt wcond.info.ty exp;
            checkType wbody.info.ty Types.UNIT "While body should be of type UNIT" exp;
            mkVal (WhileExp {wcond; wbody}) Types.UNIT
    | AssignExp {avar; aval} ->
            let avar = typecheck_var(avar, venv, tenv)
            and aval = typecheck_exp(aval, venv, tenv) in
            checkType avar.info.ty aval.info.ty 
                ("Expression has type: " ^ Types.str avar.info.ty ^ 
                 ", but variable has type: " ^ Types.str aval.info.ty) 
                exp;
            mkVal (AssignExp {avar; aval}) avar.info.ty
    | LetExp {decs; lbody} ->
            let (decs, venv, tenv) = List.fold_left 
                    (fun (decs, venv, tenv) dec -> 
                        let (dec, venv, tenv) = typecheck_dec(dec, venv, tenv) in
                        (dec :: decs, venv, tenv)
                    ) ([], venv, tenv) decs 
            in
            let lbody = typecheck_exp(lbody, venv, tenv) in
            mkVal (LetExp {decs=List.rev decs; lbody}) lbody.info.ty
    | CallExp crec ->
            let (name, formals, result) = 
                match S.look(venv, crec.call_name) with
                | Some (FunEntry (name, formals, result)) -> (name, formals, result)
                | _ -> error exp (S.name crec.call_name ^ " is not of function type")
            in
            let flen = List.length formals
            and plen = List.length crec.call_params in
            let call_params = 
                let rec checkParams acc = function
                    | ([], [], _) -> acc
                    | (param :: ptl, fty :: ftl, num) ->
                        let param = typecheck_exp(param, venv, tenv) in
                        checkType param.info.ty fty
                            ("Argument " ^ string_of_int num ^ " is of the wrong type, got " ^
                             Types.str param.info.ty ^ " expected " ^ Types.str fty)
                            exp;
                        checkParams (param :: acc) (ptl, ftl, num+1)
                    | _ -> error exp ("Function takes " ^ string_of_int flen ^ " arguments " ^
                                      "but was given " ^ string_of_int plen)
                in
                checkParams [] (crec.call_params, formals, 0)
            in
            let crec = {crec with call_params=List.rev call_params} in
            mkVal (CallExp crec) Types.UNIT
    | ArrayExp arec ->
            let arr_type = lookupTy(tenv, arec.arr_type, exp, 
                                    "Array type " ^ S.name arec.arr_type ^ " is not defined")
            in
            let arr_size = typecheck_exp(arec.arr_size, venv, tenv)
            and arr_init = typecheck_exp(arec.arr_init, venv, tenv) in
            checkType arr_size.info.ty Types.INT 
                ("Array size must be an integer, got " ^ Types.str arr_size.info.ty) exp;
            let tyinit = actualTy arr_init.info.ty in
            let tyelem = actualTy arr_type in
            match tyelem with
            | Types.ARRAY(tyelem, _) ->
                    checkType tyinit tyelem ("Initialization expression does not match " ^ 
                                             "array type\n  init type: " ^ Types.str tyinit ^
                                             "\n  elem type: " ^ Types.str tyelem) exp;
                    let arec = {arr_type=arec.arr_type; arr_size; arr_init} in
                    mkVal (ArrayExp arec) arr_type
            | _ -> ice ("ArrayExp of " ^ Types.str tyelem)
and typecheck_dec (dec, venv, tenv : dec_in * venv * tenv) : (dec_out * venv * tenv) =
    let open Ast in
    match dec with
    | FunctionDec lst ->
            (* Process headers first *)
            let msg ty = "Type " ^ S.name ty ^ " does not exist in this scope" in
            let fieldtypes lst =
                let (symlst, tylst) = List.fold_left (fun (slst, tlst) frec ->
                    let ty = lookupTy(tenv, frec.nval.ptype, frec, msg frec.nval.ptype) in
                    (frec.nval.ptype :: slst, ty :: tlst)
                ) ([], []) lst 
            in
            let (venv, _) = List.fold_left 
                (fun (acc, seen) frec ->
                    let strname = S.name frec.nval.fun_name in
                    if SS.mem strname seen then (
                        error frec ("Function " ^ strname ^ " already defined in this mutual recursion group");
                    );
                    let (_, ftype) = fieldtypes frec.nval.fun_params in
                    let rtype = match result with
                              | Some (ty, pos) -> lookupTy(tenv, ty, pos, (msg ty))
                              | None -> Types.UNIT
                    in
                    let funlabel = Temp.newlabel () in
                    (* TODO - XXX - This is wrong fix the bool list *)
                    let formals = 
                        List.fold_left (fun acc param ->
                            !(param.pescape) :: acc 
                        ) [] fields |> List.rev
                    in
                    let funlevel = T.newLevel level funlabel formals in
                    (* Add formals to the environment *)
                    let acc = S.enter (acc, name, Env.FunEntry (funlevel, funlabel, ftype, rtype)) in
                    (acc, SS.add strname seen)
                ) (venv, SS.empty) lst
            in
            let _ = List.iter (fun ((name, fields, result, body), _ : 'a A.fundec) ->
                    (* Need to introduce all of the formals as variables now *)
                    let (slst, tlst) = fieldtypes fields in
                    let level = match S.look(venv, name) with 
                        | Some (Env.FunEntry (level, _, _, _)) -> level 
                        | _ -> failwith "ICE" 
                    in
                    let venv = 
                        let formals = T.formals level in
                        List.fold_left2 (fun venv (sym, ty) access ->
                            S.enter (venv, sym, Env.VarEntry (access, ty))
                        ) venv (List.combine slst tlst) formals
                    in
                    let (bexp, tybody) = transExp (venv, tenv, level, loop, body) in
                    let tyres = match result with
                              | Some (ty, pos) -> lookup_ty tenv ty pos (msg ty)
                              | None -> Types.UNIT
                    in
                    checkType tybody tyres ("Function " ^ (Symbol.name name) ^ "'s body does not match result type") pos;
                    T.procEntryExit level bexp
                ) lst
            in
            (* TODO - lots more needs to be done for functions *)
            (*
            let lst = fold_rev (fun acc dec ->
                        let fun_body = typecheck_exp(dec.nval.fun_body, venv, tenv) in   
                        {nval={}; info={
                    ) [] lst
            in
            *)
            (FunctionDec [], venv, tenv)
    | VarDec vrec ->
            let vinit = typecheck_exp(vrec.nval.vinit, venv, tenv) in
            begin match vrec.nval.vtype with
            | Some ty -> (match S.look(tenv, ty) with
                         | Some ty -> checkType vinit.info.ty ty 
                                      "Variable type does not match expression type" vrec
                         | None -> error vrec ("Type " ^ S.name ty ^ 
                                               " does not exist in this scope"))
            | None -> ()
            end;
            let ent = VarEntry vinit.info.ty in
            let venv = S.enter(venv, vrec.nval.var_name, ent) in
            let vcheck = {vrec with nval=A.SimpleVar vrec.nval.var_name} in
            let _ = typecheck_var(vcheck, venv, tenv) in
            (VarDec {nval={vrec.nval with vinit}; info={pos=vrec.info; ty=vinit.info.ty}}, venv, tenv)
    | TypeDec lst ->
            (* Process names (for recursive types) *)
            let tenv = 
                List.fold_left (fun tenv trec ->
                    let tname = trec.nval.typ_name in
                    S.enter(tenv, tname, Types.NAME(tname, ref None))
                ) tenv lst
            in
            (* Process bodies, update the name references *)
            let lst = fold_rev (fun acc trec ->
                    let tyrec = transTy(tenv, trec.nval.type_.nval, trec) in
                    let tyref = match S.look(tenv, trec.nval.typ_name) with
                                | Some (Types.NAME(_, r)) -> r
                                | _ -> ice "TypeDec with no reference"
                    in
                    tyref := Some tyrec.info.ty;
                    tyrec :: acc
                ) [] lst
            in
            (TypeDec lst, venv, tenv)
and typecheck_var (var, venv, tenv : var_in * venv * tenv) : var_out =
    let open Ast in
    let mkVal = mkVal var in
    match var.nval with
    | SimpleVar sym -> 
            begin match S.look(venv, sym) with
            | Some (VarEntry ty) -> mkVal (SimpleVar sym) ty
            | Some (FunEntry _) -> ice "SimpleVar as a FunEntry"
            | None -> error var ("Var " ^ S.name sym ^ " does not exist in this scope")
            end
    | FieldVar frec -> 
            let fvar = typecheck_var(frec.field, venv, tenv) in
            let fty = actualTy fvar.info.ty in
            begin match fty with
            | Types.RECORD(fields, _) ->
                let rec findfield = function
                    | [] -> error var ("Field " ^ S.name frec.field_name ^ " does not exist")
                    | (fname, fty) :: tl -> if fname=frec.field_name then () else findfield tl
                in
                findfield fields
            | _ -> error var ("Field selection can only be done on record types, not " ^ 
                              Types.str fty)
            end;
            mkVal (FieldVar {frec with field=fvar}) fty
    | SubscriptVar srec ->
            let avar = typecheck_var(srec.array, venv, tenv) in
            let idx  = typecheck_exp(srec.aexp, venv, tenv) in
            checkInt idx.info.ty var;
            begin match avar.info.ty with
            | Types.ARRAY _ -> ()
            | ty -> error var ("Subscripts can only be used on array types, not " ^ Types.str ty)
            end;
            mkVal (SubscriptVar {array=avar; aexp=idx}) Types.UNIT

let typecheck (ast : ast_in) : ast_out =
    (* Add standard library functions to the default environment *)
    let venv = 
        List.fold_left (fun venv (name, args, ret) ->
            S.enter(venv, S.symbol name, FunEntry (name, args, ret))
        ) (S.empty()) Builtin.stdlib_funs
    in
    (* Add built-in types like int and string *)
    let tenv =
        List.fold_left (fun tenv (name, ty) ->
            S.enter(tenv, S.symbol name, ty)
        ) (S.empty()) Builtin.base_types
    in
    typecheck_exp(ast, venv, tenv)
;;
