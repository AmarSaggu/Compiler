{
open Parser
exception SyntaxError of string
}

let int = '-'? ['0'-'9']+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let variable = ['a'-'z']+

rule read =
    parse
    | white { read lexbuf }
    | newline { read lexbuf }
    | variable { VARIABLE (Lexing.lexeme lexbuf) }
    | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | '+'   { PLUS }
    | '-'   { MINUS }
    | '*'   { TIMES }
    | '/'   { DIVIDE }
    | '('   { LEFT_BRACE }
    | ')'   { RIGHT_BRACE }
    | '='   { ASSIGNMENT }
    | ','   { COMMA }
    | _     { raise (SyntaxError ("Unexpected char: " ^
                                  Lexing.lexeme lexbuf)) }
    | eof   { EOF }
