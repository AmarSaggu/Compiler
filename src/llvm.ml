module String = Core.Std.String
open Syntax

let indent str =
    "\t" ^
    (String.split str ~on:'\n'
    |> String.concat ~sep:"\n\t")

(*
let main_fun (ret, main) =
    "define i32 @main()\n{\n" ^ (indent (main ^ "\nret i32 ") ^ ret) ^ "\n}"
*)

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

let define_args args = 
    let prefix = "i32 " in
    List.map (fun str -> "i32 " ^ str) args
    |> String.concat ~sep:", "

let define_header name args = 
    "define i32 @" ^ name ^ "(" ^ args ^ ")"

let first = function
    | hd::tl -> hd
    | [] -> failwith "no element in empty list"

let rec last = function
    | [a] -> a
    | hd::tl -> last tl
    | [] -> failwith "no element in empty list"

let rec define_fun name args body = 
    let header = define_header name (define_args args) in
    let body = eval "x" body in
    let return = "ret i32 " ^ (fst body) in
    "\n" ^ header ^ "\n{\n" ^ (indent ((snd body) ^ "\n")) ^ return ^ "\n}\n"

and eval reg = function
    | Integer i ->
        let decl = if reg = "" then assign_reg () else "%" ^ reg in
        (decl, decl ^ " = add i32 " ^ string_of_int i ^ ", 0\n")
    | Variable v ->
        let decl = if reg = "" then assign_reg () else "%" ^ reg in
        (decl, decl ^ " = add i32 %" ^ v ^ ", 0\n")
    | Arithmetic (op, x, y) -> arith reg (Arithmetic (op, x, y))
    | Function (name, args, body) ->
        ("", define_fun name args body)
    | EList el ->
        let decl = fst (syn_ast (first (List.rev el))) in
        let def =
            List.map syn_ast el
            |> List.map snd
            |> Bytes.concat "" in
        (decl, def)
        
and syn_ast = function
    | VarDecl (str, expr) -> eval str expr
