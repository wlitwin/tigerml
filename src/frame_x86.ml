module T = Tree

type location = InReg of Temp.temp | InFrame of int

let wordsize = 4
let numRegisters = 6
let eax = Temp.newtemp ()
let ebx = Temp.newtemp ()
let ecx = Temp.newtemp ()
let edx = Temp.newtemp ()
let esp = Temp.newtemp ()
let ebp = Temp.newtemp ()

let fp = ebp
let rv = eax

let registers = ["eax"; "ebx"; "ecx"; "edx"]

type formal = location

type access = location

type register = string

type frame = 
    { label: Temp.label;
      locals: access list ref;
      local_offset: int ref; (* Becomes more negative *)
      formals: formal list }


type frag = STRING of Temp.label * string
          | FUNCTION of Tree.stm * frame

(* These lists must not overlap, and must include all registers
 * that may appear inside assembly instructions 
 *)
let special_regs = [esp; ebp]
let argregs = []
let calleesave_regs = [ebx; ecx; edx]
let callersave_regs = [eax]

let precolored = 
    let colors = Temp.ITable.empty () in
    Temp.ITable.enter (colors, eax, ());
    Temp.ITable.enter (colors, ebx, ());
    Temp.ITable.enter (colors, ecx, ());
    Temp.ITable.enter (colors, edx, ());
    Temp.ITable.enter (colors, ebp, ());
    Temp.ITable.enter (colors, esp, ());
    colors
;;

let tempMap : register Temp.ITable.table =
    let sym = Temp.ITable.empty () in
    Temp.ITable.enter (sym, eax, "eax");
    Temp.ITable.enter (sym, ebx, "ebx");
    Temp.ITable.enter (sym, ecx, "ecx");
    Temp.ITable.enter (sym, edx, "edx");
    Temp.ITable.enter (sym, ebp, "ebp");
    Temp.ITable.enter (sym, esp, "esp");
    sym
;;

let string_of_temp temp =
    match Temp.ITable.look (tempMap, temp) with
    | Some str -> str
    | None -> Temp.makestring temp
;;

let genString (label, str) =
    (Symbol.name label) ^ " .ascii " ^ str
;;

let procEntryExit1 (frame, stm) =
    print_endline "==== FORMALS ====";
    List.iter (fun loc ->
        match loc with
        | InReg reg -> print_endline (string_of_temp reg)
        | InFrame offset -> print_endline ("Frame: " ^ (string_of_int offset))
    ) frame.formals;
    T.seq ([
        T.LABEL frame.label;
        (* Save all registers *)
        T.MOVE (T.TEMP ebp, T.TEMP esp);
        (* Restore all registers, except eax *)
    ] @ [stm] @ [
        T.MOVE (T.TEMP esp, T.TEMP ebp);
    ])
;;

(* Appends a sink instruction to the function body to
 * tell the register allocator that certain registers
 * are live on exit 
 *)
let procEntryExit2 (frame, body) =
    body @ 
    [Assem.OPER {Assem.assem=""; src=[eax;esp];
     dst=[]; jump=None}]
;;

type pe3_rec = {prolog: string; body: Assem.instr list; epilog: string}
let procEntryExit3 (frame, instrs) = 
    { prolog = ""; (*(Symbol.name frame.label) ^ ":"; *)
      body = instrs; 
      epilog = "ret" }
;;

let addfragment frag =
    ()
;;

let newFrame label escapes =
    (* Make a list of all the formals *)
    let num_reg_params = 0 in (* Pass all parameters on the stack for now *)
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

