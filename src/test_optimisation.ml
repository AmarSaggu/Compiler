open Optimisation
open Syntax

type result = Pass | Fail

let compare (raw, expected) = 
    let opt = optimise [raw] in
    if opt = [expected] then Pass else Fail

let cfold = [
    (Arith (Add, (Int 5), (Int 1)), Int 6);
    (Arith (Sub, (Int 10), (Int 7)), Int 3);
    (Arith (Mul, (Int 4), (Int 5)), Int 20);

    (Arith (Div, (Int 14), (Int 3)), Int 4);
    (Arith (Div, (Int 15), (Int 3)), Int 5);
    (Arith (Div, (Int 16), (Int 3)), Int 5);

    (Arith (Add, (Int 2), (Arith (Mul, (Int 3), (Int 5)))), Int 17);
]

let ifprop = [
    (IfElse ((Int 1), (Int 2), (Int 3)), Int 2);
    (IfElse ((Int 0), (Int 2), (Int 3)), Int 3);
]

let dprop = [
    (Decl ("x", (Int 2)));
    (Decl ("y", Arith (Add, (Var "x"), (Int 1))));
];;

(*List.iter print_endline (ast_to_str (chelper dprop))*)

let x = cprop dprop in
List.map ast_to_str x
|> List.iter print_endline;;

let results =
    List.map compare cfold @
    List.map compare ifprop
let failed = List.filter ((=) Fail) results

let run () = 
    if (List.length failed) = 0 then
        print_endline "Optimisation tests passed"
    else
        print_endline "Optimisation test(s) failed"
