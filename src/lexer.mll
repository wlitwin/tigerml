{
    type pos = int
    type span = pos * pos

    let lineNum = ErrorMsg.lineNum
    let linePos = ErrorMsg.linePos

    let err pos msg = 
        ErrorMsg.error pos msg
    ;;

    let eof () = 
        (*let pos = List.hd (!linePos) in*)
        (*Tiger.EOF (pos, pos)*)
        Tiger.EOF
    ;;

    let lstart buf = 
        Lexing.lexeme_start buf
    ;;

    let lend buf =
        Lexing.lexeme_end buf
    ;;

    let lpos buf =
        (lstart buf, lend buf)
    ;;

    let illegal_character buf =
        let msg = 
            "Illegal character: " ^
            (String.make 1 (Lexing.lexeme_char buf 0))
        in
        err (lpos buf) msg
    ;;

    let increment_line_number lexbuf func =
        incr lineNum; 
        linePos := (lstart lexbuf) :: !linePos; 
        func lexbuf 
    ;;
}

    let digit = ['0'-'9']
    let letter = ['a'-'z''A'-'Z''_']
    let whitespace = [' ' '\t' '\r' '\x0c']
    let num = digit+
    let id = letter (letter | digit)*

rule token = parse
    (* Whitespace *)
      whitespace+ {  token lexbuf }
    | '\n'        { Lexing.new_line lexbuf; increment_line_number lexbuf token }
    (* Comments *)
    | "/*"        { comment 0 lexbuf }
    (* Strings *)
    | "\""        { Tiger.STRING (read_string "" lexbuf, (lpos lexbuf)) }
    (* Keywords *) 
    | "type"      { Tiger.TYPE (lpos lexbuf) }
    | "var"       { Tiger.VAR (lpos lexbuf) }
    | "function"  { Tiger.FUNCTION (lpos lexbuf) }
    | "break"     { Tiger.BREAK (lpos lexbuf) }
    | "of"        { Tiger.OF (lpos lexbuf) }
    | "end"       { Tiger.END (lpos lexbuf) }
    | "in"        { Tiger.IN (lpos lexbuf) }
    | "nil"       { Tiger.NIL (lpos lexbuf) }
    | "let"       { Tiger.LET (lpos lexbuf) }
    | "do"        { Tiger.DO (lpos lexbuf) }
    | "to"        { Tiger.TO (lpos lexbuf) }
    | "for"       { Tiger.FOR (lpos lexbuf) }
    | "while"     { Tiger.WHILE (lpos lexbuf) }
    | "else"      { Tiger.ELSE (lpos lexbuf) }
    | "if"        { Tiger.IF (lpos lexbuf) }
    | "then"      { Tiger.THEN (lpos lexbuf) }
    | "array"     { Tiger.ARRAY (lpos lexbuf) }
    | "&"         { Tiger.AND (lpos lexbuf) }
    | "|"         { Tiger.OR (lpos lexbuf) }
    | ":="        { Tiger.ASSIGN (lpos lexbuf) }
    | ">="        { Tiger.GE (lpos lexbuf) }
    | ">"         { Tiger.GT (lpos lexbuf) }
    | "<="        { Tiger.LE (lpos lexbuf) } 
    | "<"         { Tiger.LT (lpos lexbuf) }
    | "<>"        { Tiger.NEQ (lpos lexbuf) }
    | "="         { Tiger.EQ (lpos lexbuf) }
    | "/"         { Tiger.DIVIDE (lpos lexbuf) }
    | "*"         { Tiger.TIMES (lpos lexbuf) }
    | "-"         { Tiger.MINUS (lpos lexbuf) }
    | "+"         { Tiger.PLUS (lpos lexbuf) }
    | "."         { Tiger.DOT (lpos lexbuf) }
    | "}"         { Tiger.RBRACE (lpos lexbuf) }
    | "{"         { Tiger.LBRACE (lpos lexbuf) }
    | "]"         { Tiger.RBRACK (lpos lexbuf) }
    | "["         { Tiger.LBRACK (lpos lexbuf) }
    | ")"         { Tiger.RPAREN (lpos lexbuf) }
    | "("         { Tiger.LPAREN (lpos lexbuf) }
    | ";"         { Tiger.SEMICOLON (lpos lexbuf) }
    | ":"         { Tiger.COLON (lpos lexbuf) }
    | ","         { Tiger.COMMA (lpos lexbuf) }
    | id          { Tiger.ID (Lexing.lexeme lexbuf, lpos lexbuf) }  
    | num         { Tiger.INT (int_of_string (Lexing.lexeme lexbuf), lpos lexbuf) }
    | _           { illegal_character lexbuf; token lexbuf } 
    | eof         { Tiger.EOF }
and comment level = parse
    | "/*"        { comment (level + 1) lexbuf }
    | '\n'        { Lexing.new_line lexbuf; increment_line_number lexbuf (comment level) } 
    | "*/"        { if level = 0 then token lexbuf else comment (level - 1) lexbuf }
    | _           { comment level lexbuf }
    | eof         { err (lpos lexbuf) "Unterminated comment"; raise ErrorMsg.Error }
and read_string str = parse
    | "\""        { str }
    | "\\t"       { read_string (str ^ "\t") lexbuf }
    | "\\n"       { read_string (str ^ "\n") lexbuf }
    | "\\\""      { read_string (str ^ "\"") lexbuf }
    | "\\r"       { read_string (str ^ "\r") lexbuf }
    | "\\b"       { read_string (str ^ "\b") lexbuf }
    | "\\f"       { read_string (str ^ "\x0c") lexbuf }
    (* TODO - Add ASCII escape sequence *)
    |  _          { read_string (str ^ (Lexing.lexeme lexbuf)) lexbuf }
