{
open Parser
open Lexing

exception LexicalError of string
}

let number = ['0'-'9']+
let string = '"' [^'"']+ '"'
let name = ['a'-'z' 'A'-'Z']+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
    parse
    | white     { read lexbuf }
    | newline   { Lexing.new_line lexbuf; read lexbuf }

    | "fun"     { LAMBDA }

    | "if"      { IF }
    | "else"    { ELSE }

    | "true"    { NUMBER 1 }
    | "false"   { NUMBER 0 }

    | "=="      { EQ }
    | "!="      { NE }
    | "<="      { LE }
    | ">="      { GE }
    | '<'       { LT }
    | '>'       { GT }

    | '('       { LBRACE }
    | ')'       { RBRACE }

    | '{'       { LCURLY }
    | '}'       { RCURLY }

    | '+'       { ADD }
    | '-'       { SUB }
    | '*'       { MUL }
    | '/'       { DIV }

    | '#'       { comment lexbuf }

    | ','       { COMMA }
    | '='       { ASSIGNMENT }
    | name      { IDENT (Lexing.lexeme lexbuf) }
    | number    { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
    | _         { raise (LexicalError ("unexpected char '" ^
                                      Lexing.lexeme lexbuf ^
                                      "'")) }
    | eof       { EOF }

and comment =
    parse
    | newline   { Lexing.new_line lexbuf; read lexbuf }
    | eof       { EOF }
    | _         { comment lexbuf }
