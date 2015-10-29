type op =
    | Add
    | Sub
    | Mul
    | Div

type booleanop =
    | Eq
    | Neq
    | Lt
    | Gt
    | Le
    | Ge

type expr =
    | Integer of int
    | Boolean of bool
    | String of string
    | Function of string list * expr
    | Execution of string * expr list
    | Arithmetic of op * expr * expr
    | BooleanOp of booleanop * expr * expr
    | Variable of string

type ast =
    | VarDecl of string * expr

let op_str = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let comparisonops_str = function
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="

let rec expr_str = function
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | String s -> "\"" ^ s ^ "\""
    | Function (vars, f) ->
        let vars' = Bytes.concat " " vars in
        "(fun " ^ vars' ^ " -> " ^ (expr_str f) ^ ")"
    | Execution (var, args) ->
        let args' = Bytes.concat " "(List.map expr_str args) in
        var ^ " " ^ args'
    | Arithmetic (op, e, f) ->
        let op' = op_str op in
        let e' = expr_str e in
        let f' = expr_str f in
        "(" ^ e' ^ " " ^ op' ^ " " ^ f' ^ ")"
    | BooleanOp (op, e, f) ->
        let op' = comparisonops_str op in
        let e' = expr_str e in
        let f' = expr_str f in
        "(" ^ e' ^ " " ^ op' ^ " "^ f' ^ ")"
    | Variable v -> v

let ast_str = function
    | VarDecl (name, e) -> name ^ " = " ^ (expr_str e)
