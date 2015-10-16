{
open Parser
open Lexing

exception LexicalError of string
}

let int = '-'? ['0'-'9']+
let string = '"' [^'"']+ '"'
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let variable = ['a'-'z']+

rule read =
    parse
    | white     { read lexbuf }
    | newline   { Lexing.new_line lexbuf; read lexbuf }
    | variable  { VAR (Lexing.lexeme lexbuf) }
    | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | string    { STRING (let s = Lexing.lexeme lexbuf in
                          Bytes.sub s 1 ((Bytes.length s) - 2)) }
    | '+'       { PLUS }
    | '-'       { MINUS }
    | '*'       { TIMES }
    | '/'       { DIVIDE }
    | '('       { LEFT_BRACE }
    | ')'       { RIGHT_BRACE }
    | '='       { EQUALS }
    | _         { raise (LexicalError ("unexpected char '" ^
                                      Lexing.lexeme lexbuf ^
                                      "'")) }
    | eof       { EOF }
