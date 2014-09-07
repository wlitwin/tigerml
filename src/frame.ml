module T = Tree

type location = InReg of Temp.temp | InFrame of int

let wordsize = 4
let fp = Temp.newtemp ()
let rv = Temp.newtemp ()

type formal = location

type access = location

type register = string

let tempMap: (register, Temp.temp) Hashtbl.t =
    let sym = Hashtbl.create 10 in
    sym
;;

let procEntryExit1 (frame, stm) =
    T.EXP (T.CONST 0) 
;;

let procEntryExit2 (frame, instr) =
    []
;;

type frame = 
    { label: Temp.label;
      view_shift: unit;
      locals: access list ref;
      local_offset: int ref; (* Becomes more negative *)
      formals: formal list }

type frag = STRING of Temp.label * string
          | FUNCTION of Tree.stm * frame

let addfragment frag =
    ()
;;

let newFrame label escapes =
    (* Make a list of all the formals *)
    let num_reg_params = 4 in
    let rec build_formals spots offset acc = function
        | [] -> List.rev acc
        | esc :: tl ->
            if (spots > 0) && (not esc) then (
                let out = InReg (Temp.newtemp ()) in
                build_formals (spots-1) offset (out :: acc) tl
            ) else (
                let out = InFrame offset in
                build_formals spots (offset+4) (out :: acc) tl
            )
    in
    let formals = 
        build_formals num_reg_params 0 [] escapes 
    in
    { label = Temp.newlabel ();
      view_shift = ();
      locals = ref [];
      local_offset = ref 0;
      formals }
;;

let name frame =
    frame.label
;;

let formals frame : access list =
    frame.formals
;;

let allocLocal frame escape =
    let loc = match escape with
            | true -> InReg (Temp.newtemp ())
            | false ->
                    let off = !(frame.local_offset) in
                    frame.local_offset := off - 4;
                    InFrame off
    in
    frame.locals := loc :: !(frame.locals);
    loc
;;

let exp access texp : Tree.exp =
    match access with
    | InReg tmp -> T.TEMP tmp
    | InFrame off -> T.MEM (T.BINOP (T.PLUS, texp, T.CONST off))
;;

let externalCall (s, args) =
    T.CALL (T.NAME (Temp.namedlabel s), args)
;;
