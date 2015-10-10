open Core.Std

let rec print_vars = function
    | [] -> ()
    | (v,i) :: t -> print_endline (v ^ " = " ^ (string_of_int i)); print_vars t;;

let loop filename () = 
    let file = In_channel.create filename in
    Lexing.from_channel file
    |> Parser.top Lexer.read
    |> print_vars;
    print_newline ()

let () =
    Command.basic ~summary:"Compile!"
        Command.Spec.(empty +> anon ("filename" %: file))
        loop 
    |> Command.run
