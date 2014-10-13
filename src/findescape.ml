type depth = int
type escEnv = (depth * bool ref) Symbol.table

open Absyn

exception VarNotFound

let rec traverseVar (env : escEnv) (d : depth) (var : pos Absyn.var) : unit =
    match var with
    | SimpleVar (sym, pos) -> 
            let (vdepth, vref) = 
                match Symbol.look (env, sym) with
                | Some value -> value
                | None -> 
                        (* TODO - Rearrange things so this isn't possible *)
                        ErrorMsg.error pos ("Var " ^ (Symbol.name sym) ^ " does not exist in this scope - 1");
                        raise VarNotFound
            in
            if vdepth > d then vref := true
    | FieldVar (var, _, _) ->
            traverseVar env d var
    | SubscriptVar (var, exp, b) ->
            traverseVar env d var;
            traverseExp env d exp
and traverseExp (env : escEnv) (d : depth) (exp : pos Absyn.exp) : unit =
    match exp with
    | VarExp (var, _) -> traverseVar env d var
    | NilExp _ -> ()
    | IntExp _ -> ()
    | StringExp _ -> ()
    | CallExp ((_, params), _) ->
            List.iter (fun p -> traverseExp env d p) params
    | OpExp ((lexp, _, rexp), _) ->
            traverseExp env d lexp;
            traverseExp env d rexp
    | RecordExp ((lst, _), _) ->
            List.iter (fun (_, exp) -> traverseExp env d exp) lst
    | SeqExp (lst, _) ->
            List.iter (fun exp -> traverseExp env d exp) lst
    | AssignExp ((var, exp), _) ->
            traverseVar env d var;
            traverseExp env d exp
    | IfExp ((t, c, a), _) ->
            traverseExp env d t;
            traverseExp env d c;
            (match a with 
                | Some a -> traverseExp env d a
                | None -> ()
            )
    | WhileExp ((cond, body), _) ->
            traverseExp env d cond;
            traverseExp env d body
    | ForExp ((sym, ref, lo, hi, body), _) ->
            let env = Symbol.enter (env, sym, (d, ref)) in
            traverseExp env d lo;
            traverseExp env d hi;
            traverseExp env d body
    | BreakExp _ -> ()
    | LetExp ((decs, body), _) ->
            let env = traverseDecs env d decs in
            List.iter (fun exp -> traverseExp env d exp) body
    | ArrayExp ((_, size, init), _) ->
            traverseExp env d size;
            traverseExp env d init
and traverseDecs (env : escEnv) (d : depth) (dlst : pos Absyn.dec list) : escEnv =
    match dlst with
    | (VarDec ((sym, ref, _, _), _)) :: tl ->
            let env = Symbol.enter (env, sym, (d, ref)) in
            traverseDecs env d tl
    | (FunctionDec (lst, _)) :: tl ->
            List.iter (fun ((_, params, _, body), _ : pos fundec) -> 
                let penv : escEnv = 
                    List.fold_left (fun (env : escEnv) ((field, esc, _), _ : pos field) ->
                        Symbol.enter (env, field, (d, esc));
                    ) env params
                in
                traverseExp penv (d+1) body) lst;
            traverseDecs env d tl
    | _ :: tl -> 
            traverseDecs env d tl 
    | [] -> env
;;

let findescape (exp : 'a Absyn.exp) : unit =
    let env : escEnv = Symbol.empty () in
    traverseExp env 0 exp
;;
