type op =
    | Add
    | Sub
    | Mul
    | Div

type expr =
    | Integer of int
    | String of string
    | Function of (string list * expr)
    | Arithmetic of op * expr * expr
    | Variable of string

type ast =
    | VarDecl of string * expr

let print_op = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let rec print_expr = function
    | Integer i -> string_of_int i
    | String s -> "\"" ^ s ^ "\""
    | Function (vars, f) ->
        let vars' = Bytes.concat " " vars in
        "(fun " ^ vars' ^ " -> " ^ (print_expr f) ^ ")"
    | Arithmetic (op,e,f) ->
        let e' = print_expr e in
        let f' = print_expr f in
        let op' = print_op op in
        "("^e'^" "^op'^" "^f'^")"
    | Variable v -> v

let print_ast = function
    | VarDecl (name, e) -> "var " ^ name ^ " = " ^ (print_expr e)
