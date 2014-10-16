module type T =
    functor (F : Frame.Frame) ->
sig
    type level
    type access (* Not the same as Frame.access *)

    type exp

    val outermost : level
                 (* parent   name          formals *)
    val newLevel : level -> Temp.label -> bool list -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access

    val print : exp -> unit

    (* Translation functions *)
    val simpleVar : access -> level -> exp
    val fieldVar : exp -> (Symbol.symbol * Types.ty) list -> Symbol.symbol -> exp
    val subscriptVar : exp -> exp -> exp

    val procEntryExit : level -> exp -> unit
    val getResult : unit -> F.frag list

    val intExp : int -> exp
    val nilExp : unit -> exp
    val strExp : string -> exp
    val ifExp : exp -> exp -> exp -> exp
    val seqExp : exp list -> exp
    val recordExp : exp list -> exp
    val assignExp : exp -> exp -> exp
    val whileExp : exp -> exp -> Temp.label -> exp
    val breakExp : Temp.label -> exp
    val arrayExp : exp -> exp -> exp
    val callExp : Temp.label -> exp list -> level -> level -> exp
    val callExtern : Temp.label -> exp list -> level -> level -> exp

    val arithExp : Absyn.oper -> exp -> exp -> exp
    val condExp : Absyn.oper -> exp -> exp -> exp
end

module Make : T = functor (Frame : Frame.Frame) ->
struct

type exp = Ex of Tree.exp
         | Nx of Tree.stm
         | Cx of (Temp.label * Temp.label -> Tree.stm)

type level = { prev: level option; frame: Frame.frame; uniq: unit ref }

type access = level * Frame.access

let outermost : level = { prev = None; 
                          frame = Frame.newFrame (Temp.newlabel ()) []; 
                          uniq = ref () }

let frag_list : Frame.frag list ref = ref []

let addFragment frag =
    frag_list := frag :: !frag_list
;;

let getResult () : Frame.frag list =
    !frag_list
;;

let formals (lev : level) : access list =
    let frmls = Frame.formals lev.frame in
    (* TODO - If some functions don't need the static link update this *)
    (* Need to remove the one we added in newLevel *)
    List.tl (List.map (fun frml -> (lev, frml)) frmls)
;;

let allocLocal lev escape =
    let access = Frame.allocLocal lev.frame escape in
    (lev, access)
;;

let newLevel lev lab formals =
    (* Add new formal parameter that represents the static link
     * this parameter always escapes *)
    let formals = true :: formals in
    let frame = Frame.newFrame lab formals in
    { prev = Some lev; frame; uniq = ref () }
;;

module T = Tree

let unEx (e : exp) : Tree.exp =
    match e with
    | Ex e -> e
    | Cx genstm ->
            let r = Temp.newtemp ()
            and t = Temp.newlabel ()
            and f = Temp.newlabel () in
            T.ESEQ (T.seq [T.MOVE (T.TEMP r, T.CONST 1);
                          genstm (t, f);
                          T.LABEL f;
                          T.MOVE (T.TEMP r, T.CONST 0);
                          T.LABEL t],
                    T.TEMP r)
    | Nx s -> T.ESEQ (s, T.CONST 0)
;;

let unNx (e : exp) : Tree.stm =
    match e with
    | Ex e -> T.EXP e
    | Cx genstm -> 
            let tf = Temp.newlabel () in
            T.SEQ (genstm (tf, tf), T.LABEL tf)
    | Nx s -> s
;;

let unCx (e : exp) : (Temp.label * Temp.label -> Tree.stm) =
    match e with
    | Ex (T.CONST 0) -> (fun (t, f) -> T.JUMP (T.NAME f, [f]))
    | Ex (T.CONST 1) -> (fun (t, f) -> T.JUMP (T.NAME t, [t]))
    | Ex e -> (fun (t, f) -> T.CJUMP (T.EQ, e, T.CONST 0, f, t))
    | Cx genstm -> genstm
    | Nx _ -> failwith "Impossible condition in unCx. Should not happen in well typed programs."
;;

let print exp =
    Print_tree.print (unNx exp)
;;

let procEntryExit level body =
    if (level.uniq == outermost.uniq) then
        raise (Failure "Shouldn't be using outermost level")
    ;
    let body = T.MOVE (T.TEMP Frame.rv, unEx body) in
    let result = Frame.procEntryExit1 (level.frame, body) in
    addFragment (Frame.FUNCTION (result, level.frame))
;;

(* Translation functions *)
let simpleVar (varlvl, faccess) curlevel : exp =
    (* If we're at the same level we can use Frame.fp *) 
    (* If not, we have to calculate where it is via static links *)
    if varlvl.uniq == curlevel.uniq then    
        Ex (Frame.exp faccess (T.TEMP Frame.fp))
    else (
        (* Create nested MEM's *)
        let rec generate_mems acc texp lvl =
            if lvl.uniq == varlvl.uniq then (
                Frame.exp faccess texp  
            ) else (
                let sloc = List.hd (Frame.formals lvl.frame) in
                let texp = Frame.exp acc texp in 
                generate_mems sloc texp lvl
            )
        in
        let sloc = List.hd (Frame.formals curlevel.frame) in
        Ex (generate_mems sloc (T.TEMP Frame.fp) curlevel)
    )
;;

let subscriptVar baseExp indexExp =
    (* TODO add code to check for out of bounds access *)
    Ex (T.MEM (T.BINOP (T.PLUS, 
                        unEx baseExp, 
                        T.BINOP (T.MUL, 
                                 unEx indexExp, 
                                 T.CONST Frame.wordsize))))
;;

let fieldVar recExp flst fname =
    let rec offset acc = function
        | [] -> acc
        | (hd, _) :: tl ->
                if fname = hd then acc
                else offset (acc+Frame.wordsize) tl
    in
    let fieldOffset = offset 0 flst in
    (* TODO - Check if record is nil *)
    Ex (T.MEM (T.BINOP (T.PLUS, 
                        unEx recExp, 
                        T.CONST fieldOffset)))
;;

let intExp num =
    Ex (T.CONST num)
;;

let nilExp () =
    Ex (T.CONST 0)
;;

let strExp str =
    let str_lbl = Temp.newlabel () in
    addFragment (Frame.STRING (str_lbl, str));
    Ex (T.NAME str_lbl)
;;

let ifExp c t e =
    let open Tree in
    let c = unCx c
    and t_lbl = Temp.newlabel ()
    and f_lbl = Temp.newlabel ()
    and d_lbl = Temp.newlabel () in
    (* TODO - Better code-gen
    match (t, e) with
    | (Nx t, Nx e) ->
            Nx (T.seq [genstm (t_lbl, f_lbl);
                       
                      ])
    | (Ex t, Ex e) -> ()
    *)
    let t = unEx t
    and e = unEx e
    and r = Temp.newtemp () in
    Ex (ESEQ (seq 
            [c (t_lbl, f_lbl);
             LABEL f_lbl;
             MOVE (TEMP r, e);
             JUMP (NAME d_lbl, [d_lbl]);
             LABEL t_lbl;
             MOVE (TEMP r, t);
             JUMP (NAME d_lbl, [d_lbl]);
             LABEL d_lbl],
            TEMP r));
;;

let seqExp lst =
    let rec build_seq  = function
        | [] -> T.CONST 0 (*failwith "Compile Error: seqExp impossible condition"*)
        | [exp] -> unEx exp
        | e1 :: tl -> T.ESEQ (unNx e1, build_seq tl)
    in
    Ex (build_seq lst)
;;

let recordExp fields = 
    let flen = List.length fields in
    let r = Temp.newtemp () in
    let (_, initExps) = List.fold_left
        (fun (off, lst) fexp ->
            let fexp = unEx fexp in
            let init = T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP r, T.CONST off)), 
                               fexp) 
            in
            (off + Frame.wordsize, init :: lst)
        ) (0, []) fields
    in
    Ex (T.ESEQ (
        T.seq 
            ((T.MOVE (T.TEMP r, Frame.externalCall ("allocRecord", [T.CONST (flen*Frame.wordsize)])))
            ::
            initExps), T.TEMP r))
;;

let assignExp left right =
    Nx (T.MOVE (unEx left, unEx right))
;;

let whileExp cond body lbl_done =
    let lbl_test = Temp.newlabel ()
    and lbl_body = Temp.newlabel ()
    and tmp_cond = Temp.newtemp ()
    and cond = unEx cond
    and body = unNx body in
    Nx (T.seq [T.LABEL lbl_test;
               T.MOVE (T.TEMP tmp_cond, cond);
               T.CJUMP (T.EQ, T.TEMP tmp_cond, T.CONST 0, lbl_done, lbl_body);
               T.LABEL lbl_body;
               body;
               T.JUMP (T.NAME lbl_test, [lbl_test]);
               T.LABEL lbl_done;
              ])
;;

let breakExp lbl =
    Nx (T.JUMP (T.NAME lbl, [lbl]))
;;

let arrayExp size init =
    Ex (Frame.externalCall ("initArray", [unEx size; unEx init]))
;;

(* lof - level of called function *)
(* locf - level of calling function *)
let callExp (name : Temp.label) (params : exp list) (lof : level) (locf : level) =
    let params = List.map unEx params in
    (* TODO - calculate static link offset *)
    Ex (T.CALL (T.NAME name, (T.CONST 0) :: params))
;;

let callExtern (name : Temp.label) (params : exp list) (lof : level) (locf : level) =
    let params = List.map unEx params in
    Ex (T.CALL (T.NAME name, params))
;;

let arithExp op expl expr =
    let open Absyn in
    let expl = unEx expl
    and expr = unEx expr in
    match op with
    | PlusOp -> Ex (T.BINOP (T.PLUS, expl, expr))
    | MinusOp -> Ex (T.BINOP (T.MINUS, expl, expr))
    | TimesOp -> Ex (T.BINOP (T.MUL, expl, expr))
    | DivideOp -> Ex (T.BINOP (T.DIV, expl, expr))
    | _ -> failwith "Compiler Error: wrong op used in arithExp"
;;

let condExp op expl expr =
    let open Absyn in
    let expl = unEx expl
    and expr = unEx expr in
    let mkFun op = fun (t, f) -> 
        T.CJUMP (op, expl, expr, t, f)
    in
    match op with
    | LtOp -> Cx (mkFun T.LT)
    | LeOp -> Cx (mkFun T.LE)
    | GtOp -> Cx (mkFun T.GT)
    | GeOp -> Cx (mkFun T.GE)
    | EqOp -> Cx (mkFun T.EQ)
    | NeqOp -> Cx (mkFun T.NE)
    | _ -> failwith "Compiler Error: wrong op used in condExp"
;;

end
