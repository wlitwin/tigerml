module type T =
    functor (Frame : Frame.Frame) ->
sig
    type allocation = (Frame.register, Temp.temp) Hashtbl.t
    type data = {interference : Liveness.igraph;
                 initial: allocation;
                 spillCost: Graph.node -> int;
                 registers: Frame.register list}
    val color : data -> allocation * Temp.temp list
end

module Make : T = functor (Frame : Frame.Frame) ->
struct 
    type allocation = (Frame.register, Temp.temp) Hashtbl.t
    type data = {interference : Liveness.igraph;
                 initial: allocation;
                 spillCost: Graph.node -> int;
                 registers: Frame.register list}

    let addEdge (u, v) =
        if (not SS.mem (u, v) adjSet) && (u != v) then begin
            adjSet <- SS.add (u, v) adjSet;
            adjSet <- SS.add (v, u) adjSet;
            if (u != precolored) then begin

            end
            if (v != precolored) then begin

            end
        end
    ;;

    let main () =
        livenessAnaylsis ();
        build ();
        makeworklist ();
        let rec loop () =
            if simplifyWorklist != SS.empty then simplify ()
            else if worklistMoves != SS.empty then coalesce ()
            else if freezeWorklist != SS.empty then freeze ()
            else if spillWorklist != SS.empty then selectSpill ()
            ;
            if (simplifyWorklist != SS.empty || 
                worklistMoves != SS.empty ||
                freezeWorklist != SS.empty || 
                spillWorklist != SS.empty)
            then begin
                   loop ()
            end 

        in
        loop ()
    ;;

    let color (data : data) : allocation * Temp.temp list =
        (Hashtbl.create 10, [])
    ;;
end
