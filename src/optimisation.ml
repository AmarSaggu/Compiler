open Syntax

let op_to_char = function
    | Add -> ( + )
    | Sub -> ( - )
    | Mul -> ( * )
    | Div -> ( / )

let rec constant_fold exp = match exp with
    | Arithmetic (op, Integer x, Integer y) ->
            Integer ((op_to_char op) x y)
    | Arithmetic (op, Integer x, y) ->
            Arithmetic (op, Integer x, constant_fold y)
    | Arithmetic (op, x, Integer y) ->
            Arithmetic (op, constant_fold x, Integer y)
    | Arithmetic (op, x, y) ->
            Arithmetic (op, constant_fold x, constant_fold y)
    | _ -> exp

let cfold' = function
    | VarDecl (s, e) -> VarDecl (s, constant_fold e)

let rec cfold ast =
    let ast' = cfold' ast in
    if ast = ast' then ast
    else cfold ast'
