type op =
    | Add
    | Sub
    | Mul
    | Div

type expr =
    | Integer of int
    | Boolean of bool
    | String of string
    | Function of (string list * expr)
    | Arithmetic of op * expr * expr
    | Variable of string

type ast =
    | VarDecl of string * expr

let op_to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let rec expr_to_string = function
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | String s -> "\"" ^ s ^ "\""
    | Function (vars, f) ->
        let vars' = Bytes.concat " " vars in
        "(fun " ^ vars' ^ " -> " ^ (expr_to_string f) ^ ")"
    | Arithmetic (op,e,f) ->
        let e' = expr_to_string e in
        let f' = expr_to_string f in
        let op' = op_to_string op in
        "(" ^ e' ^ " " ^ op' ^ " " ^ f' ^ ")"
    | Variable v -> v

let print_ast = function
    | VarDecl (name, e) -> name ^ " = " ^ (expr_to_string e)
