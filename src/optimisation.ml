open Syntax

let op_to_func = function
    | Add -> ( + )
    | Sub -> ( - )
    | Mul -> ( * )
    | Div -> ( / )

let cop_to_func = function
    | Eq -> ( = )
    | Ne -> ( <> )
    
    | Lt -> ( < )
    | Gt -> ( > )
    | Le -> ( <= )
    | Ge -> ( >= ) 

(*
let rec cfold = function
    | Function (name, args, body) ->
        let body' = cfold body in
        Function (name, args, body')
    | Call (name, args) ->
        let args' = List.map cfold args in
        Call (name, args')
    
    | Int i -> Int i
    | Arith (op, Int x, Int y) ->
            Int ((op_to_func op) x y)
    | Arith (op, x, y) ->
            Arith (op, cfold x, cfold y)
 
    | Comp (cop, Int a, Int b) ->
        Int ((cop_to_func cop) a b)
    | Comp (cop, a, b) ->
        Comp (cop, cfold a, cfold b)

    | Decl (name, body) ->
        let body' = cfold body in
        Decl (name, body')
    | Var v -> Var v

    | EList body ->
        let body' = List.map cfold body in
        EList body'

    | IfElse (cond, a, b) ->
        let cond' = cfold cond in
        let a' = cfold a in
        let b' = cfold b in
        IfElse (cond', a', b')

let rec cfold' ast =
    let ast' = List.map cfold ast in
    if ast = ast' then
        ast
    else
        cfold' ast'
*)

let rec cover f ast = 
    let cf = cover f in
    match ast with
    | Function (name, args, body) ->
        f (Function (name, args, cf body))
    | Call (name, args) ->
        f (Call (name, List.map cf args))

    | Int i -> f (Int i)
    | Arith (op, a, b) ->
        f (Arith (op, cf a, cf b))
    | Comp (cop, a, b) ->
        f (Comp (cop, cf a, cf b))

    | Decl (name, body) ->
        f (Decl (name, cf body))
    | Var v -> f (Var v)
    
    | EList el ->
        f (EList (List.map cf el))

    | IfElse (cond, a, b) ->
        f (IfElse (cf cond, cf a, cf b))

let int_of_bool = function
    | true -> 1
    | false-> 0

let cfold = function
    | Arith (op, Int x, Int y) ->
        Int ((op_to_func op) x y)
 
    | Comp (cop, Int a, Int b) ->
        let result = (cop_to_func cop) a b in
        Int (int_of_bool result)

    | a -> a

(*
let cprop = function
    | EList el ->
        let reduce lst = function
            | Decl (name, Int i) ->
                [name, i] :: lst
            | a -> lst in
        let env = List.fold_left reduce [] el in

        List.map ast_to_str el
        |> List.map ((^) "\t")
        |> List.iter print_endline;
        EList el
    | a -> a
*)

let cpreg = function
    | EList el -> 
        let play = function
            | 

let cprop = function
    | EList el ->
        let reduce lst = function
            | Decl (name, Int i) ->
                [name, i] :: lst
            | a -> lst in
        let env = List.fold_left reduce [] el in

        let replace = fun x -> match x with
            | Var v -> match List.Assoc.Find env v with
                | Some i -> Integer i
                | None -> Var v in
        
        cover replace 

        List.map ast_to_str el
        |> List.map ((^) "\t")
        |> List.iter print_endline;
        EList el
    | a -> a

let ifprop = function
    | IfElse (Int 0, _, b) ->
        b
    | IfElse (Int _, a, _) ->
        a
    | a -> a

let optimise prog = 
    let opt astl = 
           List.map (cover cfold) astl
        |> List.map (cover cprop) 
        |> List.map (cover ifprop) in
    let prog' = opt prog in
    if prog = prog' then prog
    else opt prog'
