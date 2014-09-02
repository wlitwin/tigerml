module A = Assem

let codegen frame (stm : Tree.stm) : Assem.instr list =
    let ilist = ref ([] : A.instr list) in
    let emit x = ilist := x :: !ilist in
    let result gen = 
        let t = Temp.newtemp () in
        gen t; t
    in
    let munchStm = function
        | _ -> ()
    and munchExp = function
        | _ -> ()
    in
    munchStm stm;
    List.rev !ilist
;;
