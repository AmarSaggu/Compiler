open Core.Std
open Syntax

let indent str =
    "\t" ^
    (String.split str ~on:'\n'
    |> String.concat ~sep:"\n\t")

let main_fun (ret, main) =
    "define i32 @main()\n{\n" ^ (indent (main ^ "\nret i32 ") ^ ret) ^ "\n}"

let assign_reg =
    let reg = ref 0 in
    fun () ->
        incr reg;
        "%" ^ (string_of_int !reg)

let op_to_opcode = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "sdiv"

let rec arith reg = function
    | Integer i -> (string_of_int i, "")
    | Variable v -> ("%" ^ v, "")
    | Arithmetic (op, x, y) ->
        let (xdecl, xdef) = arith "" x in
        let (ydecl, ydef) = arith "" y in
        let op_str = op_to_opcode op in
        let decl = if reg = "" then assign_reg () else "%" ^ reg in
        let def = decl ^ " = " ^ op_str ^ " i32 " ^ xdecl ^ ", " ^ ydecl ^ "\n" in
        (decl, xdef ^ ydef ^ def)

let eval reg = function
    | Integer i ->
        let decl = if reg = "" then assign_reg () else "%" ^ reg in
        (decl, decl ^ " = add i32 " ^ string_of_int i ^ ", 0\n")
    | Variable v ->
        let decl = if reg = "" then assign_reg () else "%" ^ reg in
        (decl, decl ^ " = add i32 %" ^ v ^ ", 0\n")
    | Arithmetic (op, x, y) -> arith reg (Arithmetic (op, x, y))

let syn_ast = function
    | VarDecl (str, expr) -> eval str expr
