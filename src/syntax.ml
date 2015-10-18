type op =
    | Add
    | Sub
    | Mul
    | Div

type exp =
    | IntExp of int
    | VarExp of string
    | Exp of op * exp * exp

type types =
    | Int of exp
    | Str of string

type ast =
    | Stuff of ast list
    | VarDecl of string * types
    | Var of string

let print_op = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let rec print_exp = function
    | IntExp i -> string_of_int i
    | VarExp v -> v
    | Exp (op,e,f) ->
        let e' = print_exp e in
        let f' = print_exp f in
        let op' = print_op op in
        "("^e'^" "^op'^" "^f'^")"

let print_type = function
    | Int i -> print_exp i
    | Str s -> "\"" ^ s ^ "\""

let print_ast = function
    | VarDecl (name,value) -> "var " ^ name ^ " = " ^ (print_type value)
    | Var v -> "value of " ^ v
