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
    (Symbol.name label) ^ 
    " dd " ^ (string_of_int (String.length str)) ^ "\n" ^
    " db `" ^ (String.escaped str) ^ "`"

;;

let allocLocal frame escape =
    let loc = match escape with
            | false -> InReg (Temp.newtemp ())
            | true ->
                    let off = !(frame.local_offset) in
                    frame.local_offset := off - 4;
                    InFrame off
    in
    frame.locals := loc :: !(frame.locals);
    loc
;;

let procEntryExit1 (frame, stm) =
    print_endline "==== FORMALS ====";
    List.iter (fun loc ->
        match loc with
        | InReg reg -> print_endline (string_of_temp reg)
        | InFrame offset -> print_endline ("Frame: " ^ (string_of_int offset))
    ) frame.formals;
    let numLocals = List.length !(frame.locals) in
    let lst = [
            T.LABEL frame.label;
            (* Save all registers *)
            T.MOVE (T.TEMP ebp, T.TEMP esp);
            T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP esp, T.CONST ~-4)), T.TEMP ebx);
            T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP esp, T.CONST ~-8)), T.TEMP ecx);
            T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP esp, T.CONST ~-12)), T.TEMP edx);
            T.MOVE (T.TEMP esp, T.BINOP (T.PLUS, T.TEMP esp, T.CONST ~-12));
        ] @ 
        (if numLocals > 0 then
            (* Create space for locals *)
            [T.MOVE (T.TEMP esp, T.BINOP (T.PLUS, T.TEMP esp, T.CONST (-4*numLocals)))]
        else [])
        (* Restore all registers, except eax *)
        @ [stm] @ [
            T.MOVE (T.TEMP esp, T.TEMP ebp);
            T.MOVE (T.TEMP ebx, T.MEM (T.BINOP (T.PLUS, T.TEMP esp, T.CONST ~-4)));
            T.MOVE (T.TEMP ecx, T.MEM (T.BINOP (T.PLUS, T.TEMP esp, T.CONST ~-8)));
            T.MOVE (T.TEMP edx, T.MEM (T.BINOP (T.PLUS, T.TEMP esp, T.CONST ~-12)));
        ]
    in
    T.seq lst
;;

(* Appends a sink instruction to the function body to
 * tell the register allocator that certain registers
 * are live on exit 
 *)
let procEntryExit2 (frame, body) =
    match body with 
    | hd :: tl ->
        hd :: (Assem.OPER  {Assem.assem="push `s0"; dst=[]; src=[ebp]; jump=None}) :: tl
        @ [Assem.OPER {Assem.assem="; Sink Instruction"; src=[eax;esp;ebp]; dst=[]; jump=None}]
    | _ -> failwith "ICE"
;;

type pe3_rec = {prolog: string; body: Assem.instr list; epilog: string}
let procEntryExit3 (frame, instrs) = 
    { prolog = ""; (*(Symbol.name frame.label) ^ ":"; *)
      body = instrs; 
      epilog = "leave\nret" }
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
        build_formals num_reg_params 4 [] escapes 
    in
    { label;
      locals = ref [];
      local_offset = ref ~-16;
      formals }
;;

let name frame =
    frame.label
;;

let formals frame : access list =
    frame.formals
;;

let exp access texp : Tree.exp =
    match access with
    | InReg tmp -> T.TEMP tmp
    | InFrame off -> T.MEM (T.BINOP (T.PLUS, texp, T.CONST off))
;;

let externalCall (s, args) =
    T.CALL (T.NAME (Temp.namedlabel s), List.rev args)
;;
