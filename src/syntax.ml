type op =
    | Add
    | Sub
    | Mul
    | Div

type bop =
    | Eq
    | Ne

    | Lt
    | Gt
    | Le
    | Ge

type ast =
    | Function of string * string list * ast
    | Call of string * ast list
    
    | Int of int
    | Arith of op * ast * ast
    | Comp of bop * ast * ast
    
    | Decl of string * ast
    | Var of string
    
    | EList of ast list

    | IfElse of ast * ast * ast

let op_to_str = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let bop_to_str = function
    | Eq -> "=="
    | Ne -> "!="

    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="

let rec ast_to_str = function
    | Function (name, vars, body) ->
        let vars' = Bytes.concat " " vars in
        name ^ " = fun " ^ vars' ^ " -> " ^ (ast_to_str body)
    | Call (func, args) ->
        let args' = Bytes.concat " "(List.map ast_to_str args) in
        func ^ "(" ^ args' ^ ")"
   
    | Int i -> string_of_int i  
    | Arith (op, e, f) ->
        let op' = op_to_str op in
        let e' = ast_to_str e in
        let f' = ast_to_str f in
        "(" ^ e' ^ " " ^ op' ^ " " ^ f' ^ ")"
    | Comp (bop, e, f) ->
        let bop' = bop_to_str bop in
        let e' = ast_to_str e in
        let f' = ast_to_str f in
        "(" ^ e' ^ " " ^ bop' ^ " " ^ f' ^ ")"   

    | Decl (name, value) -> name ^ " = " ^ (ast_to_str value)
    | Var v -> v
    
    | EList el ->
        let el' = Bytes.concat "\n" (List.map ast_to_str el) in
        "{\n" ^ el' ^ "\n}"

    | IfElse (cond, a, b) ->
        let cond' = ast_to_str cond in
        let a' = ast_to_str a in
        let b' = ast_to_str b in
        "if " ^ cond' ^ " then " ^ a' ^ " else " ^ b'
