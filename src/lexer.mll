{
open Parser
open Lexing

exception LexicalError of string
}

let number = ['0'-'9']+
let string = '"' [^'"']+ '"'
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['a'-'z']+

rule read =
    parse
    | white     { read lexbuf }
    | newline   { Lexing.new_line lexbuf; read lexbuf }
    | "fun"     { LAMBDA }
    | "->"      { ARROW }
    | '+'       { ADD }
    | '-'       { SUB }
    | '*'       { MUL }
    | '/'       { DIV }
    | ','       { COMMA }
    | '='       { EQUALS }
    | '('       { LBRACE }
    | ')'       { RBRACE }
    | name      { IDENT (Lexing.lexeme lexbuf) }
    | number    { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
    | string    { STRING (let s = Lexing.lexeme lexbuf in
                          Bytes.sub s 1 ((Bytes.length s) - 2)) }
    | _         { raise (LexicalError ("unexpected char '" ^
                                      Lexing.lexeme lexbuf ^
                                      "'")) }
    | eof       { EOF }
