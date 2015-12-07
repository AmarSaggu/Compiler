module Command = Core.Std.Command
module In_channel = Core.Std.In_channel
module Out_channel = Core.Std.Out_channel

open Llvm

open Printf
open Lexing
open Bytes
open Codegen
open Lexer
open Syntax
open Optimisation

let print_ast ast = 
    List.map ast_to_str ast
    |> Bytes.concat "\n"
    |> print_endline

(*
let print_ir filename prog = 
    List.map (compile (create_reg_generator "%") (create_reg_generator "")) prog
    |> List.map fst
    |> Bytes.concat ""
    |> fun x -> Out_channel.write_all ((Filename.chop_suffix filename ".yip") ^ ".ll") ~data:x
*)

(*
let print_ir filename prog =
    List.iter Codegen.yee prog
*)


(*
let tokenise ast should_opt filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Errors.name_lexbuf filename
    |> Errors.parse file
    |> (if should_opt then optimise else (fun x -> x) )
    |> (if ast then print_ast else (print_ir filename))
*)

let yee prog =
    ignore (List.map (compile global_scope) prog);
    print_endline (string_of_llmodule mdl);
    dispose_module mdl
    
let tokenise ast should_opt filename () =
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Errors.name_lexbuf filename
    |> Errors.parse file
    |> yee

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
