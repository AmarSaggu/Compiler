module Command = Core.Std.Command
module In_channel = Core.Std.In_channel
module Out_channel = Core.Std.Out_channel

open Printf
open Lexing
open Bytes
open Llvm
open Lexer
open Syntax
open Optimisation

let print_ast prog = 
    List.map ast_to_str prog
    |> Bytes.concat "\n"
    |> print_endline

let print_ir filename prog = 
    List.map (compile (create_reg_generator "%") (create_reg_generator "")) prog
    |> List.map fst
    |> Bytes.concat ""
    |> fun x -> Out_channel.write_all ((Filename.chop_suffix filename ".yip") ^ ".ll") ~data:x

let tokenise boo should_opt filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Errors.name_lexbuf filename
    |> Errors.parse file
    |> (if should_opt then optimise else (fun x -> x) )
    |> (if boo then print_ast else (print_ir filename))

let () =
    Command.basic ~summary:"Compile"
        Command.Spec.(
            empty
            +> flag "-t" no_arg ~doc:"Display the AST"
            +> flag "-O" no_arg ~doc:"enable optimisation"
            +> anon ("filename" %: file)
        )
        tokenise
    |> Command.run
