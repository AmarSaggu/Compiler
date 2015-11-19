module Command = Core.Std.Command
module In_channel = Core.Std.In_channel
open Printf
open Lexing
open Bytes

open Llvm
open Lexer
open Syntax
open Optimisation

let tokenise filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Errors.name_lexbuf filename
    |> Errors.parse file
    |> optimise
    |> List.map ast_to_str
    |> List.iter print_endline
    (*
    |> List.map (compile (create_reg_generator "%") (create_reg_generator ""))
    |> List.map fst
    |> Bytes.concat ""
    |> print_endline
    *)

let () =
    Command.basic ~summary:"Compile"
        Command.Spec.(empty +> anon ("filename" %: file))
        tokenise 
    |> Command.run
