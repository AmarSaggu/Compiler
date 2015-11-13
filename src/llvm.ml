module String = Core.Std.String
open Syntax

let rec last = function
    | [hd] -> hd
    | hd :: tl -> last tl

let indent str = 
    "\t" ^
    (String.split str ~on:'\n'
    |> String.concat ~sep:"\n\t")

let op_to_opcode = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "sdiv"

let create_reg_generator () = 
    let register = ref 0 in
    fun () ->
        incr register;
        "%" ^ (string_of_int !register)

let rec arithmetic gen = function
    | Integer i -> ("", string_of_int i)
    | Variable v -> ("", "%" ^ v)
    | Arithmetic (op, x, y) ->
        let (xdef, xname) = arithmetic gen x in
        let (ydef, yname) = arithmetic gen y in
        let op_str = op_to_opcode op in
        let dep = "" in
        let reg = gen () in
        (xdef ^ ydef ^ reg ^ " = " ^ op_str ^ " i32 " ^ xname ^ ", " ^ yname ^ "\n", reg)

let define_args args =
    let prefix = "i32 " in
    let args' =
        List.map ((^) prefix) args
        |> String.concat ~sep:", " in
    "(" ^ args' ^ ")"

let define_header name args =
    let args_str = define_args args in
    "define i32 @" ^ name ^ args_str

let rec define_fun name args body =
    let header = define_header name args in
    let body = compile (create_reg_generator ()) body in
    let return = "ret i32 " ^ (snd body) in
    "\n" ^ header ^ "\n{\n" ^ (indent ((fst body) ^ "\n" ^ return)) ^ "\n}\n"

and compile gen = function
    | Integer i ->
        let i_str = string_of_int i in
        let reg = gen () in
        (reg ^ " = add i32 " ^ i_str ^ ", 0\n", reg)
    | Decl (name, exp) ->
        let (def, reg) = compile gen exp in
        (def ^ "%" ^ name ^ " = add i32 " ^ reg ^ ", 0\n", "%" ^ name)
    | Variable v ->
        ("", "%" ^ v)
    | Function (name, args, body) ->
        (define_fun name args body, "")
    | Call (name, args) ->
        let cargs = List.map (compile gen) args in
        let def =
            List.map fst cargs
            |> Bytes.concat "" in
        let args' =
            List.map snd cargs
            |> define_args in
        let reg = gen () in
        let call = "call i32 " ^ "@" ^ name ^ args' in
        (def ^ reg ^ " = " ^ call, reg)
    | EList el ->
        let comp = List.map (compile gen) el in
        let name = snd (last comp) in
        let def = 
            List.map fst comp
            |> Bytes.concat "" in
        (def, name)
    | Arithmetic (op, x, y) ->
        arithmetic gen (Arithmetic (op, x, y))
