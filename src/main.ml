module Command = Core.Std.Command
module In_channel = Core.Std.In_channel
open Printf
open Lexing
open Bytes

open Lexer
open Syntax;;

let tokenise filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Errors.name_lexbuf filename
    |> Errors.parse file
    |> List.map Optimisation.cfold 
    |> List.iter (fun y -> print_endline (print_ast y)) 

let () =
    Command.basic ~summary:"Compile"
        Command.Spec.(empty +> anon ("filename" %: file))
        tokenise 
    |> Command.run
