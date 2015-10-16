open Lexing
open Lexer
open Printf

type result = Pass | Fail

let tests = [
    (Pass, "");
    (Pass, "str = \"Hello World\"");
    (Fail, "str = \"Goodbye World");
    (Fail, "str");
    (Pass, "n = 2 + 4");
    (Pass, "n = 8 * 2 - 3 / 423");
    (Pass, "n = -1 * -9 - -2 / -4");
    (Fail, "n = 1 + ");
    (Pass, "n = n + 1");
    (Pass, "lsaklsjfsofaoiasfdisfho = gjafaskfdajfdsjllfasdks");
    (Fail, "str = \"hello \" + \"world\"");
    (Fail, "str = 2 * \"four\"");
    (Pass, "n = (a + 2) / (b - 1)");
    (Fail, "n = -(2 + 2)");
    (Pass, "str\n=\n2\n+\n5\n\n");
];;

let print_result = function
    | Pass -> "Pass"
    | Fail -> "Fail"

let run prog = 
    Lexing.from_string prog
    |> Parser.top Lexer.read
    |> (fun x -> ())

let test prog =
    try (run prog; Pass) with
    | LexicalError msg -> Fail
    | Parser.Error -> Fail

let compare (expect, prog) = (expect, test prog)

let print_result (expect, actual) = 
    let expect' = print_result expect in
    let actual' = print_result actual in
    "    " ^ expect' ^ " | " ^ actual';;

printf "Running %d tests\n\n" (List.length tests);;
print_endline "Expected   Actual"

let results = List.map compare tests;;
List.map print_endline (List.map print_result results);;

let passed = List.filter (fun (a,b) -> a = b) results;;
printf "\nPassed %d/%d tests\n" (List.length passed) (List.length tests)
