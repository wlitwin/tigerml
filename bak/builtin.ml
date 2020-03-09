open Types

let stdlib_funs = [
    ("print", [STRING], UNIT);
    ("flush", [], UNIT);
    ("getchar", [], STRING); 
    ("ord", [STRING], INT);
    ("chr", [INT], STRING);
    ("size", [STRING], INT);
    ("substring", [STRING; INT; INT], STRING);
    ("concat", [STRING; STRING], STRING);
    ("not", [INT], INT);
    ("exit", [], UNIT);
    ("blah", [], STRING);
    ("stringEqual", [STRING; STRING], INT)
]

let base_types = [
    ("int", Types.INT);
    ("string", Types.STRING)
]
