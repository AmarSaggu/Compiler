open Core.Std
open Printf
open Lexing
open Bytes

open Lexer
open Syntax

let loop filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Warning.name_lexbuf filename
    |> Warning.parse file
    |> (fun x -> List.iter x (fun y -> print_endline (print_ast y)))

let () =
    Command.basic ~summary:"Compile"
        Command.Spec.(empty +> anon ("filename" %: file))
        loop 
    |> Command.run
