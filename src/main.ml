module Command = Core.Std.Command
module In_channel = Core.Std.In_channel
open Printf
open Lexing
open Bytes

open Llvm
open Lexer
open Syntax

let tokenise filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Errors.name_lexbuf filename
    |> Errors.parse file
    |> List.iter (fun y -> print_endline (main_fun (syn_ast y)))

let () =
    Command.basic ~summary:"Compile"
        Command.Spec.(empty +> anon ("filename" %: file))
        tokenise 
    |> Command.run
