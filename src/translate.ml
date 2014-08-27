module T = Tree

type exp = Ex of Tree.exp
         | Nx of Tree.stm
         | Cx of (Temp.label * Temp.label -> Tree.stm)

type level = { prev: level option; frame: Frame_x86.frame; uniq: unit ref }

type access = level * Frame_x86.access

let outermost : level = { prev = None; 
                          frame = Frame_x86.newFrame (Temp.newlabel ()) []; 
                          uniq = ref () }

let frag_list : Frame_x86.frag list ref = ref []

let addFragment frag =
    frag_list := frag :: !frag_list
;;

let getResult () : Frame_x86.frag list =
    !frag_list
;;

let formals (lev : level) : access list =
    let frmls = Frame_x86.formals lev.frame in
    (* TODO - If some functions don't need the static link update this *)
    (* Need to remove the one we added in newLevel *)
    List.tl (List.map (fun frml -> (lev, frml)) frmls)
;;

let allocLocal lev escape =
    let access = Frame_x86.allocLocal lev.frame escape in
    (lev, access)
;;

let newLevel lev lab formals =
    (* Add new formal parameter that represents the static link
     * this parameter always escapes *)
    let formals = true :: formals in
    let frame = Frame_x86.newFrame lab formals in
    { prev = Some lev; frame; uniq = ref () }
;;

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

let procEntryExit level exp =
    ()
;;

(* Translation functions *)
let simpleVar (varlvl, faccess) curlevel : exp =
    (* If we're at the same level we can use Frame_x86.fp *) 
    (* If not, we have to calculate where it is via static links *)
    if varlvl.uniq == curlevel.uniq then    
        Ex (Frame_x86.exp faccess (T.TEMP Frame_x86.fp))
    else (
        (* Create nested MEM's *)
        let rec generate_mems acc texp lvl =
            if lvl.uniq == varlvl.uniq then (
                Frame_x86.exp faccess texp  
            ) else (
                let sloc = List.hd (Frame_x86.formals lvl.frame) in
                let texp = Frame_x86.exp acc texp in 
                generate_mems sloc texp lvl
            )
        in
        let sloc = List.hd (Frame_x86.formals curlevel.frame) in
        Ex (generate_mems sloc (T.TEMP Frame_x86.fp) curlevel)
    )
;;

let subscriptVar baseExp indexExp =
    (* TODO add code to check for out of bounds access *)
    Ex (T.MEM (T.BINOP (T.PLUS, 
                        T.MEM (unEx baseExp), 
                        T.MEM (T.BINOP (T.MUL, 
                                        (unEx indexExp), 
                                        T.CONST Frame_x86.wordsize)))))
;;

let fieldVar recExp flst fname =
    let rec offset acc = function
        | [] -> acc
        | (hd, _) :: tl ->
                if fname = hd then acc
                else offset (acc+4) tl
    in
    let fieldOffset = offset 0 flst in
    (* TODO - Check if record is nil *)
    Ex (T.MEM (T.BINOP (T.PLUS, 
                        T.MEM (unEx recExp), 
                        T.CONST (fieldOffset * Frame_x86.wordsize))))
;;

let intExp num =
    Ex (T.CONST num)
;;

let nilExp () =
    Ex (T.CONST 0)
;;

let strExp str =
    let str_lbl = Temp.newlabel () in
    addFragment (Frame_x86.STRING (str_lbl, str));
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
             LABEL t_lbl;
             MOVE (TEMP r, t);
             JUMP (NAME d_lbl, [d_lbl]);
             LABEL f_lbl;
             MOVE (TEMP r, e);
             JUMP (NAME d_lbl, [d_lbl]);
             LABEL d_lbl],
            TEMP r));
;;

let seqExp lst =
    let rec build_seq = function
        | [] -> failwith "Compile Error: seqExp impossible condition"
        | [exp] -> unNx exp
        | e1 :: tl -> T.SEQ (unNx e1, build_seq tl)
    in
    Nx (build_seq lst)
;;

let recordExp fields = 
    let r = Temp.newtemp () in
    let flen = List.length fields in
    let (_, initExps) = List.fold_left
        (fun (off, lst) fexp ->
            let fexp = unEx fexp in
            let init = T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP r, T.CONST (off*Frame_x86.wordsize))), 
                               fexp) 
            in
            (off + Frame_x86.wordsize, init :: lst)
        ) (0, []) fields
    in
    Ex (T.ESEQ (
        T.seq 
            ((T.MOVE (T.TEMP r, Frame_x86.externalCall ("malloc", [T.CONST (flen*Frame_x86.wordsize)])))
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
    Ex (Frame_x86.externalCall ("initArray", [unEx size; unEx init]))
;;

(* lof - level of called function *)
(* locf - level of calling function *)
let callExp (name : Temp.label) (params : exp list) (lof : level) (locf : level) =
    let params = List.map unEx params in
    (* TODO - calculate static link offset *)
    Ex (T.CALL (T.NAME name, (T.CONST 0) :: params))
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
    let expl = unNx expl
    and expr = unEx expr in
    let blah = (fun (t, f) -> expl) in
    (* TODO - finish condExp *)
    match op with
    | LtOp -> Cx blah
    | LeOp -> Cx blah
    | GtOp -> Cx blah
    | GeOp -> Cx blah
    | EqOp -> Cx blah
    | NeqOp -> Cx blah
    | _ -> failwith "Compiler Error: wrong op used in condExp"
;;
