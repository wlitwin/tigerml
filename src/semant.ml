module type T = 
    functor (F : Frame.Frame) 
            (T : (module type of Translate.Make (F)))
            (E : (module type of Env.Make (F) (T))) ->
sig

type venv = E.enventry Symbol.table
type tenv = Types.ty Symbol.table

exception Error
exception Impossible

type expty = T.exp * Types.ty

type pos = Absyn.pos

val transProg : pos Absyn.exp -> F.frag list
val transTy   :        tenv * pos Absyn.ty  -> Types.ty
val transVar  : venv * tenv * T.level * Temp.label option * pos Absyn.var -> expty
val transExp  : venv * tenv * T.level * Temp.label option * pos Absyn.exp -> expty
val transDec  : venv * tenv * T.level * Temp.label option * pos Absyn.dec -> 
               (venv * tenv * T.exp list)

end

module Make : T = 
    functor (F   : Frame.Frame)
            (T   : (module type of Translate.Make (F)))
            (Env : (module type of Env.Make (F) (T))) ->
struct

module A = Absyn
module S = Symbol
module SS = Set.Make(String)

type venv = Env.enventry S.table
type tenv = Types.ty S.table

type expty = T.exp * Types.ty

type pos = Absyn.pos

let dummy_pos = (-1, -1)

exception Error
exception Impossible

let is_some = function 
    | Some _ -> true
    | _ -> false
;;

let some opt =
    match opt with
    | Some v -> v
    | _ -> raise Error
;;

let is_none opt =
    not (is_some opt)
;;

let error pos msg =
    ErrorMsg.error pos msg;
    raise Error
;;

let impossible msg =
    print_endline msg;
    raise Impossible
;;

let lookup_ty tbl ty pos msg =
    match S.look (tbl, ty) with
    | Some ty -> ty
    | None -> error pos msg
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

let checkInt ty pos =
    checkType ty Types.INT "Expected integer expression" pos
;;

let rec actual_ty : Types.ty -> Types.ty = function
    | Types.NAME (_, op) -> (match !op with
        | Some ty -> actual_ty ty
        | None -> print_endline "Invalid name object"; raise Error)
    | Types.ARRAY (ty, uniq) -> Types.ARRAY (actual_ty ty, uniq)
    | ty -> ty
;;

(* Break statements need to know if they're in a loop or not *)
let rec transVar (venv, tenv, level, loop, var) : expty = match var with
    | A.SimpleVar (sym, pos) -> 
            (match S.look (venv, sym) with
            | Some (Env.VarEntry (acc, ty)) -> 
                    (T.simpleVar acc level, actual_ty ty)
            | Some (Env.ExtFunEntry _)
            | Some (Env.FunEntry _) -> impossible "SimpleVar as a FunEntry"
            | None -> error pos ("Var " ^ (S.name sym) ^ " does not exist in this scope - 2")
            )
    | A.FieldVar (var, sym, pos) ->
            let (vexpty, varty) = transVar (venv, tenv, level, loop, var) in
            let varty = actual_ty varty in 
            (* Should be a record type and have this field in it *)
            (match varty with
            | Types.RECORD (lst, _) ->
                    let rec findfield = function
                        | [] -> error pos ("Field " ^ (S.name sym) ^ " does not exist")
                        | (fname, fty) :: tl ->
                                if fname = sym then (T.fieldVar vexpty lst fname, fty)
                                else findfield tl
                    in
                    findfield lst
            | _ -> error pos ("Field selection can only be done on record types not " ^ 
                                (Types.str varty))
            )
    | A.SubscriptVar (var, idx, pos) ->
            let (vexpty, varty) = transVar (venv, tenv, level, loop, var) in
            let (iexpty, tyidx) = transExp (venv, tenv, level, loop, idx) in
            checkInt tyidx pos;
            let varty = actual_ty varty in
            match varty with
            | Types.ARRAY (ty, _) -> (T.subscriptVar vexpty iexpty, ty)
            | _ -> error pos ("Subscripts can only be used on array types not " ^ 
                                (Types.str varty))

and transExp (venv, tenv, level, loop, exp) : expty = 
    match exp with
    | A.NilExp _ -> (T.nilExp (), Types.NIL)
    | A.IntExp (num, _) -> (T.intExp num, Types.INT)
    | A.StringExp (str, _) -> (T.strExp str, Types.STRING)
    (**************** Binary Expressions ****************)
    | A.OpExp ((left, op, right), pos) ->
            let (lexpty, tyleft) = transExp (venv, tenv, level, loop, left)
            and (rexpty, tyright) = transExp (venv, tenv, level, loop, right) in
            (match op with
            | A.PlusOp  | A.MinusOp 
            | A.TimesOp | A.DivideOp -> 
                    checkInt tyleft pos; 
                    checkInt tyright pos;
                    (T.arithExp op lexpty rexpty, Types.INT)
            | A.EqOp  | A.LtOp | A.LeOp
            | A.NeqOp | A.GtOp | A.GeOp ->
                    let tyleft = actual_ty tyleft
                    and tyright = actual_ty tyright in
                    (match (tyleft, tyright) with
                    | (Types.INT, Types.INT)
                    | (Types.NIL, Types.NIL)
                    | (Types.STRING, Types.STRING)
                    | (Types.NIL, Types.RECORD _)
                    | (Types.RECORD _, Types.NIL)
                    | (Types.ARRAY _, Types.ARRAY _)
                    | (Types.RECORD _, Types.RECORD _) ->
                            (T.condExp op lexpty rexpty, Types.INT)
                    | _ -> error pos ("Can't compare " ^ (Types.str tyleft) ^ 
                                      " and " ^ (Types.str tyright))
                    )
            )
     (**************** If Expression ****************)
     | A.IfExp ((test, conseq, alt), pos) ->
            (* test must evaluate to an integer *)
            let (cexpty, tytest) = transExp (venv, tenv, level, loop, test) in
            checkType tytest Types.INT 
                ("If conditionals must be integer expressions, condition is of " ^ 
                    (Types.str (actual_ty tytest)) ^ " type")
                pos;
            let (texpty, tythen) = transExp (venv, tenv, level, loop, conseq) in
            let tythen = actual_ty tythen in
            (match alt with
            | Some alt ->
                let (eexpty, tyelse) = transExp (venv, tenv, level, loop, alt) in
                let tyelse = actual_ty tyelse in
                let msg = "If expressions then and else branches must have the same type" ^
                          "\n  Then has type: " ^ (Types.str tythen) ^
                          "\n  Else has type: " ^ (Types.str tyelse)
                in
                checkType tythen tyelse msg pos;
                (T.ifExp cexpty texpty eexpty, tythen)
            | None ->
                checkType tythen Types.UNIT 
                    ("If then expressions must be of unit type was " ^ (Types.str tythen))
                    pos;
                (T.ifExp cexpty texpty (T.intExp 0), tythen)
            )
     (**************** Sequence Expression ****************)
     | A.SeqExp (lst, pos) ->
            let (exptylst, tyseq) = List.fold_left
                (fun (elst, tyacc) exp ->
                    let (expty, tyexp) = transExp (venv, tenv, level, loop, exp) in
                    (expty :: elst, tyexp)
                ) ([], Types.UNIT) lst 
            in
            (T.seqExp (List.rev exptylst), tyseq)
     (**************** Record Expression ****************)
     | A.RecordExp ((fields, tyrec), pos) ->
            (* Get the type from the env *)
            let tyrec = actual_ty (lookup_ty tenv tyrec pos
                ("Record type " ^ (S.name tyrec) ^ " is not defined"))
            in
            (match tyrec with
            | Types.RECORD (rec_fields, _) ->
                    (* Check the fields *)
                    let rec fieldtype name flist : Types.ty =
                        match flist with
                        | [] -> error pos ((S.name name) ^ " does not exist for this record type")
                        | (field, ty) :: tl ->
                            if field = name then ty
                            else fieldtype name tl
                    in
                    let fields = List.fold_left 
                        (fun acc (fname, exp) -> 
                            let (fexp, tyexp) = transExp (venv, tenv, level, loop, exp) in
                            let tyfield = fieldtype fname rec_fields in
                            checkType tyexp tyfield
                                ("Exp is not of the correct type for field: " ^ 
                                    (S.name fname) ^ " = " ^ (Types.str (actual_ty tyfield))) 
                                pos;
                            (fexp :: acc)
                        ) [] fields
                    in
                    (T.recordExp (List.rev fields), tyrec)
            | _ -> error pos ("Not of record type: " ^ (Types.str (actual_ty tyrec)))
            )
     (**************** Assign Expression ****************)
     | A.AssignExp ((var, exp), pos) ->
             let (lexpty, varty) = transVar (venv, tenv, level, loop, var)
             and (rexpty, tyexp) = transExp (venv, tenv, level, loop, exp) in
             checkType tyexp varty "Expression has different type than variable" pos;
             (T.assignExp lexpty rexpty, Types.UNIT)
     (**************** While Expression ****************)
     | A.WhileExp ((cond, body), pos) ->
            let (cexpty, tycond) = transExp (venv, tenv, level, loop, cond) in
            let lbl_done = Some (Temp.newlabel ()) in
            let (bexpty, tybody) = transExp (venv, tenv, level, lbl_done, body) in
            checkInt tycond pos;
            checkType tybody Types.UNIT "While body must be of unit type" pos;
            (T.whileExp cexpty bexpty (some lbl_done), tybody)
     (**************** For Expression ****************)
     | A.ForExp ((var, esc, lo, hi, body), pos) ->
            (* TODO - Make sure var is not assigned to in the body *)
            (* TODO - Rewrite as a while loop *)
             let vlo = var
             and vhi = S.symbol "limit" in
             let vlo_exp = A.SimpleVar (vlo, pos)
             and vhi_exp = A.SimpleVar (vhi, pos) in
             let inc_exp = A.OpExp ((A.VarExp (vlo_exp, pos), A.PlusOp, A.IntExp (1, pos)), pos) in
             let plus_eq = A.AssignExp ((vlo_exp, inc_exp), pos) in
             let vlo_dec = A.VarDec ((vlo, esc, Some (S.symbol "int", pos), lo), pos)
             (* TODO does this escape? *)
             and vhi_dec = A.VarDec ((vhi, ref true, Some (S.symbol "int", pos), hi), pos) in
             (* Le -> Lt maybe *)
             let while_cond = A.OpExp ((A.VarExp (vlo_exp, pos), A.LeOp, A.VarExp (vhi_exp, pos)), pos) in
             let while_exp =
                 A.LetExp (([vlo_dec; vhi_dec],
                           [A.WhileExp ((while_cond,
                                        A.SeqExp ([body; plus_eq], pos)), pos)]),
                           pos)
             in
             transExp (venv, tenv, level, loop, while_exp)
             (*
            let vacc = T.allocLocal level !esc in
            let venv = S.enter (venv, var, Env.VarEntry (vacc, Types.INT)) in
            let (loexpty, tylo) = transExp (venv, tenv, level, loop, lo)
            and (hiexpty, tyhi) = transExp (venv, tenv, level, loop, hi) in
            let lbl_done = Temp.newlabel () in
            let (bexpty, tybody) = transExp (venv, tenv, level, lbl_done, body) in
            checkInt tylo (A.getVal lo);
            checkInt tyhi (A.getVal hi);
            checkType tybody Types.UNIT "For body must be of unit type" (A.getVal body); 
            (T.forExp loexpty hiexpty bexpty, Types.UNIT)
            *)
     (**************** Break Expression ****************)
     | A.BreakExp (_, pos) ->
            if is_none loop then error pos "Break can only be used inside of a loop";
            (T.breakExp (some loop), Types.UNIT)
     (**************** Let Expression ****************)
     | A.LetExp ((decs, body), pos) ->
            let (venv, tenv, vexps) =
                let rec lop (vn, tn, acc) lst =
                    match lst with
                    | [] -> (vn, tn, acc)
                    | d :: tl -> 
                        let (vn, tn, vexps) = transDec (vn, tn, level, loop, d) in
                        lop (vn, tn, vexps @ acc) tl
                in
                lop (venv, tenv, []) decs
            in
            (* Generate code for assignments *)
            let body = A.SeqExp (body, pos) in
            let (bexp, bexpty) = transExp (venv, tenv, level, loop, body) in
            (T.seqExp (vexps @ [bexp]), bexpty)
     (**************** Array Expression ****************)
     | A.ArrayExp ((tyarr, size, init), pos) ->
            let tyarr = lookup_ty tenv tyarr pos 
                ("Array type " ^ (S.name tyarr) ^ " is not defined")
            in
            let (sexpty, tysize) = transExp (venv, tenv, level, loop, size) in
            let (iexpty, tyinit) = transExp (venv, tenv, level, loop, init) in
            checkInt tysize pos;
            let tyinit = actual_ty tyinit in
            let tyelem = actual_ty tyarr in
            (match tyelem with
            | Types.ARRAY (tyelem, _) ->
                checkType tyinit tyelem
                    ("Initialization expression does not match array type\n  array of " ^
                    (Types.str tyelem) ^ "\n  init: " ^ (Types.str tyinit))
                    pos
            | _ -> impossible ("ArrayExp of " ^ (Types.str tyelem))
            );
            (T.arrayExp sexpty iexpty, tyarr)
     (**************** Call Expression ****************)
     | A.CallExp ((name, params), pos) ->
            let (lvl, name, formals, tyresult, isextern) =
                match S.look (venv, name) with
                | Some (Env.FunEntry (lvl, name, formals, result)) -> (lvl, name, formals, result, false)
                | Some (Env.ExtFunEntry (lvl, name, formals, result)) -> (lvl, name, formals, result, true)
                | _ -> error pos ((S.name name) ^ " is not of function type")
            in
            let flen = List.length formals 
            and plen = List.length params in
            let rec checkparams acc = function
                | ([], [], false) -> (T.callExp name (List.rev acc) lvl level, tyresult)
                | ([], [], true)  -> (T.callExtern name (List.rev acc) lvl level, tyresult)
                | (fty :: ftl, p :: ptl, isextern) ->
                    let (pexpty, pty) = transExp (venv, tenv, level, loop, p) in 
                    checkType pty fty "Argument is of the wrong type" (A.getVal p);
                    checkparams (pexpty :: acc) (ftl, ptl, isextern)
                | _ -> error pos ("Function takes " ^ (string_of_int flen) ^ 
                                  ", but was given " ^ (string_of_int plen))
            in
            checkparams [] (formals, params, isextern)
     (**************** Var Expression ****************)
     | A.VarExp (var, _) -> transVar (venv, tenv, level, loop, var)

and transDec (venv, tenv, level, loop, dec : venv * tenv * T.level * Temp.label option * pos Absyn.dec) = 
    match dec with
    | A.FunctionDec (lst, pos) ->
            (* Process the function "headers" first *)
            let msg ty = "Type " ^ (S.name ty) ^ " does not exist in this scope" in
            let fieldtypes lst =
                let (slst, tlst) = (List.fold_left 
                    (fun (slst, tlst) ((sym, _, ty), pos : 'a A.field) -> 
                        let ty = lookup_ty tenv ty pos (msg ty) in
                        (sym :: slst, ty :: tlst)) 
                    ([], []) lst)
                in (List.rev slst, List.rev tlst)
            in
            let (venv, _) = List.fold_left 
                (fun (acc, seen) ((name, fields, result, _), pos : 'a A.fundec) ->
                    let strname = S.name name in
                    if SS.mem strname seen then (
                        error pos ("Function " ^ strname ^ " already defined in this mutual recursion group");
                    );
                    let (_, ftype) = fieldtypes fields in
                    let rtype = match result with
                              | Some (ty, pos) -> lookup_ty tenv ty pos (msg ty)
                              | None -> Types.UNIT
                    in
                    let funlabel = Temp.newlabel () in
                    (* TODO - XXX - This is wrong fix the bool list *)
                    let formals = 
                        List.fold_left (fun acc ((_, esc, _), _ : pos A.field) ->
                            !esc :: acc 
                        ) [] fields |> List.rev
                    in
                    let funlevel = T.newLevel level funlabel formals in
                    (* Add formals to the environment *)
                    let acc = S.enter (acc, name, Env.FunEntry (funlevel, funlabel, ftype, rtype)) in
                    (acc, SS.add strname seen)
                ) (venv, SS.empty) lst
            in
            (* Process the function bodies now *)
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
                    checkType tybody tyres "Function body does not match result type" pos;
                    T.procEntryExit level bexp
                ) lst
            in
            (venv, tenv, [])
    | A.VarDec ((sym, ref, varty, exp), pos) ->
            let (vexp, tyexp) = transExp (venv, tenv, level, loop, exp) in
            (match varty with
            | Some (ty, pos) -> 
                    (match S.look (tenv, ty) with
                    | Some ty ->
                        checkType tyexp ty "Variable type does not match expression type" pos
                    | None ->
                        error pos ("Type " ^ (S.name ty) ^ " does not exist in this scope")
                    )
            | None -> ()
            );
            let vaccess = T.allocLocal level !ref in
            let ent = Env.VarEntry (vaccess, tyexp) in
            let venv = S.enter (venv, sym, ent) in
            let (var, _) = transVar (venv, tenv, level, loop, A.SimpleVar (sym, pos)) in
            let vexp = T.assignExp var vexp in
            (venv, tenv, [vexp])
    | A.TypeDec (lst, pos) ->
            (* Process the type names first *)
            let tenv = List.fold_left
                (fun tenv (sym, _, _) -> S.enter (tenv, sym, Types.NAME (sym, ref None)))
                tenv lst
            in
            (* Process the type bodies now *)
            let _ = List.iter (fun (sym, ty, _) -> 
                    let ty = transTy (tenv, ty) in
                    let tyref = match S.look (tenv, sym) with
                                | Some (Types.NAME (_, r)) -> r
                                | _ -> impossible "TyDec with no reference"
                    in
                    tyref := Some ty
                ) lst
            in
            (venv, tenv, [])

and transTy (tenv, ty) =
    let msg sym = "Type " ^ (S.name sym) ^ " does not exist in this scope" in
    match ty with
    | A.NameTy (sym, pos) -> lookup_ty tenv sym pos (msg sym)
    | A.RecordTy (fields, pos) ->
        let rec transFields acc = function
            | [] -> List.rev acc
            | ((name, _, ty), pos) :: tl ->
                let ty = lookup_ty tenv ty pos (msg ty) in
                transFields ((name, ty) :: acc) tl
        in
        Types.RECORD ((transFields [] fields), ref ())
    | A.ArrayTy (sym, pos) -> 
            let ty = lookup_ty tenv sym pos (msg sym) in
            Types.ARRAY (ty, ref ())
;;

let transProg (exp : pos Absyn.exp) : F.frag list =
    let entry = Temp.namedlabel "__prog" in
    let firstLevel = T.newLevel T.outermost entry [] in
    let venv = Env.base_venv firstLevel 
    and tenv = Env.base_tenv () in
(*    and exit  = Temp.newlabel () in*)
    let (ir, typ) = transExp (venv, tenv, firstLevel, None, exp) in
    print_endline ("Final program type: " ^ (Types.str typ)); 
    T.print ir;
    T.procEntryExit firstLevel ir;
    T.getResult ();
;;

end
