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

(* apply a function to every possible expression *)
let rec infect f ast = 
    let cf = infect f in
    match ast with
    | Function (name, args, body) ->
        f (Function (name, args, List.map cf body))
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

    | IfElse (cond, a, b) ->
        f (IfElse (cf cond, cf a, cf b))

(* apply a function to all expressions in a single function *)
let rec cover f ast =
    let cf = cover f in
    match ast with
    | Function (name, args, body) ->
        Function (name, args, body)
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

let rec overrite f ast = 
    let cf = overrite f in
    match ast with
    | Function (name, args, body) ->
        Function (name, args, f body)
    | Call (name, args) ->
        Call (name, f args)
    
    | Int i -> Int i
    | Arith (op, a, b) ->
        Arith (op, a, b)
    | Comp (cop, a, b) ->
        Comp (cop, a, b)

    | Decl (name, body) ->
        Decl (name, body)
    | Var v -> Var v

    | IfElse (cond, a, b) ->
        IfElse (cond, a, b)

let replace_var name value = function
    | Var v ->
        if v = name then
            value
        else
            Var v
    | a -> a

let rec cprop = function
    | Decl (name, Int i) :: tl -> 
        let tl' = List.map (cover (replace_var name (Int i))) tl in
        if tl' = [] then Decl (name, Int i) :: cprop tl' else cprop tl'
    | hd :: tl -> hd :: cprop tl
    | [] -> []


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
(*
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
        
        infect replace 

        List.map ast_to_str el
        |> List.map ((^) "\t")
        |> List.iter print_endline;
        EList el
    | a -> a
*)

let ifprop = function
    | IfElse (Int 0, _, b) ->
        b
    | IfElse (Int _, a, _) ->
        a
    | a -> a

let optimise prog = 
    let opt astl = 
           List.map (infect cfold) astl
        |> List.map (infect ifprop)
        |> List.map (overrite cprop) in
    let prog' = opt prog in
    if prog = prog' then prog
    else opt prog'
