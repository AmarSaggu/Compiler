module String = Core.Std.String

type op =
    | Add
    | Sub
    | Mul
    | Div

type cop =
    | Eq
    | Ne

    | Lt
    | Gt
    | Le
    | Ge

type ast =
    | Function of string * string list * ast
    | External of string * string list
    | Call of string * ast list
    
    | Int of int
    | Arith of op * ast * ast
    | Comp of cop * ast * ast
    
    | Decl of string * ast
    | Assign of string * ast 
    | Var of string

    | IfElse of ast * ast * ast
    
    | Block of ast list
    | Repeat
    | Return of ast


let indent str =
    "\t" ^
    (String.split str ~on:'\n'
    |> Bytes.concat "\n\t")

let op_to_str = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let cop_to_str = function
    | Eq -> "=="
    | Ne -> "!="

    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="

let rec ast_to_str = function
    | Function (name, args, body) ->
        let args = Bytes.concat " " args in
        let body = ast_to_str body in
        name ^ " = fun " ^ args ^ " ->\n" ^ (indent body) ^ "\nend\n"
    | External (name, args) ->
        let args = Bytes.concat " " args in
        "extern " ^ name ^ " = fun " ^ args ^ "\n"
    | Call (func, args) ->
        let args = Bytes.concat " " (List.map ast_to_str args) in
        func ^ "(" ^ args ^ ")"
   
    | Int i -> string_of_int i  
    | Arith (op, e, f) ->
        let op = op_to_str op in
        let e = ast_to_str e in
        let f = ast_to_str f in
        "(" ^ e ^ " " ^ op ^ " " ^ f ^ ")"
    | Comp (cop, e, f) ->
        let cop = cop_to_str cop in
        let e = ast_to_str e in
        let f = ast_to_str f in
        "(" ^ e ^ " " ^ cop ^ " " ^ f ^ ")"   
    | Decl (name, value) -> "let " ^ name ^ " = " ^ (ast_to_str value)
    | Assign (name, value) -> name ^ " = " ^ (ast_to_str value)
    | Var v -> v

    | IfElse (cond, a, b) ->
        let cond = ast_to_str cond in
        let a = ast_to_str a in
        let b = ast_to_str b in
        "if " ^ cond ^ " then \n" ^ (indent a) ^ "\nelse\n" ^ (indent b) ^ "\nend\n"

    | Block body ->
        let body =
            List.map ast_to_str body
            |> Bytes.concat " " in
        "{\n" ^ (indent body) ^ "\n}\n"
    | Repeat -> "repeat\n"
    | Return r ->
        let r = ast_to_str r in
        "ret " ^ r
