open Core.Std
open Printf
open Lexing
open Lexer
open Syntax
open Bytes

let get_line file line_num =
    In_channel.seek file (Int64.of_int line_num);
    match In_channel.input_line file with
        | Some line -> line
        | None -> ""

let name_lexbuf filename lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
        { pos with pos_fname = filename };
    lexbuf

let get_file_info lexbuf =
    let pos = lexbuf.lex_curr_p in
    sprintf "%s:%d:%d: " pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let print_error file lexbuf msg =
    let fileinfo = get_file_info lexbuf in
    prerr_string (fileinfo ^ msg ^ "\n");
    let pos = lexbuf.lex_curr_p in
    let offset = pos.pos_cnum - pos.pos_bol in
    eprintf "%s\n%s^\n" (get_line file pos.pos_bol) (String.make (offset - 1) ' ')

let parse_with_error file lexbuf = 
    try Parser.top Lexer.read lexbuf with
    | LexicalError msg ->
        print_error file lexbuf "lexical error: ";
        exit (-1)
    | Parser.Error ->
        print_error file lexbuf "syntax error: ";
        exit (-1)

let loop filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> name_lexbuf filename
    |> parse_with_error file
    |> (fun x -> List.iter x (fun y -> print_endline (print_ast y)))

let () =
    Command.basic ~summary:"compile"
        Command.Spec.(empty +> anon ("filename" %: file))
        loop 
    |> Command.run
