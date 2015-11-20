open Lexing
open Lexer
open Printf
module In_channel = Core.Std.In_channel

type result = Pass | Fail

let result_to_string = function
    | Pass -> "Pass"
    | Fail -> "Fail"

let get_filenames () = Sys.readdir "tests/pass"
let get_file filename = In_channel.create filename

let run prog = 
    Lexing.from_channel prog
    |> Parser.top Lexer.read
    |> (fun x -> ())

let test prog =
    try (run prog; Pass) with
    | LexicalError msg -> Fail
    | Parser.Error -> Fail

let compare (expect, prog) = (expect, test prog)

let print_result expect (filename, actual) = 
    let expect' = result_to_string expect in
    let actual' = result_to_string actual in
    filename ^ ": expected to " ^ expect' ^ ", not " ^ actual';;

let run_tests dir =
    let files = Array.to_list (Sys.readdir dir) in
    List.map (Bytes.cat dir) files
    |> List.map get_file
    |> List.map test
    |> List.combine files;;

let pass = run_tests "tests/pass/";;
let fail = run_tests "tests/fail/";;

let passed = List.filter (fun x -> (snd x) != Pass) pass;;
let failed = List.filter (fun x -> (snd x) != Fail) fail;;

List.map (fun x -> print_endline (fst x)) passed;;

if (List.length passed) + (List.length failed) = 0 then
    print_endline "All tests passed"
else begin
    print_endline "Test(s) failed";
    List.iter (fun x -> print_endline (print_result Pass x)) passed;
    List.iter (fun x -> print_endline (print_result Fail x)) failed
end;

Test_optimisation.run ()
