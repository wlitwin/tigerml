val anyErrors : bool ref
val fileName : string ref
val lineNum : int ref
val linePos : int list ref
val sourceStream : char Stream.t ref
val error : (int * int) -> string -> unit
exception Error
val impossible : string -> 'a (* raises Error *)
val reset : unit -> unit

