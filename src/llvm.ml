module String = Core.Std.String
open Syntax

let rec last = function
    | [hd] -> hd
    | hd :: tl -> last tl

let indent str = 
    "\t" ^
    (String.split str ~on:'\n'
    |> Bytes.concat "\n\t")

let op_to_opcode = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "sdiv"

let create_reg_generator prefix = 
    let register = ref 0 in
    fun () ->
        incr register;
        prefix ^ (string_of_int !register)

let define_args args =
    let prefix = "i32 " in
    let args' =
        List.map ((^) prefix) args
        |> Bytes.concat ", " in
    "(" ^ args' ^ ")"

let define_header name args =
    let args_str = define_args args in
    "define i32 @" ^ name ^ args_str

let rec define_fun name args body =
    let header = define_header name args in
    let body = compile (create_reg_generator "%") (create_reg_generator "") body in
    let return = "ret i32 " ^ (snd body) in
    "\n" ^ header ^ "\n{\n" ^ (indent ((fst body) ^ "\n" ^ return)) ^ "\n}\n"

and arithmetic gen label = function
    | Integer i -> ("", string_of_int i)
    | Arithmetic (op, x, y) ->
        let (xdef, xname) = arithmetic gen label x in
        let (ydef, yname) = arithmetic gen label y in
        let op_str = op_to_opcode op in
        let dep = "" in
        let reg = gen () in
        (xdef ^ ydef ^ reg ^ " = " ^ op_str ^ " i32 " ^ xname ^ ", " ^ yname ^ "\n", reg)
    | a -> compile gen label a

and compile gen label = function
    | Integer i ->
        let i_str = string_of_int i in
        let reg = gen () in
        (reg ^ " = add i32 " ^ i_str ^ ", 0\n", reg)
    | Decl (name, exp) ->
        let (def, reg) = compile gen label exp in
        (def ^ "%" ^ name ^ " = add i32 " ^ reg ^ ", 0\n", "%" ^ name)
    | Variable v ->
        ("", "%" ^ v)

    | Function (name, args, body) ->
        let args' = List.map ((^) "%") args in
        (define_fun name args' body, "")
    | Call (name, el) ->
        let cargs = List.map (compile gen label) el in
        let def =
            List.map fst cargs
            |> Bytes.concat "" in
        let args =
            List.map snd cargs
            |> define_args in
        let reg = gen () in
        let call = " = call i32 " ^ "@" ^ name ^ args ^ "\n"in
        (def ^ reg ^ call, reg)

    | EList el ->
        let comp = List.map (compile gen label) el in
        let name = snd (last comp) in
        let def = 
            List.map fst comp
            |> Bytes.concat "" in
        (def, name)
    | Arithmetic (op, x, y) ->
        arithmetic gen label (Arithmetic (op, x, y))
    
    | IfElse (cond, x, y) ->
        let labelid = label () in
        let (cdef, cname) = compile gen label cond in
        let reg = gen () in
        let (xdef, xname) = compile gen label x in
        let (ydef, yname) = compile gen label y in
        let comp = reg ^ " = icmp ne i32 " ^ cname ^ ", 0\n" in
        let ifl = "if" ^ labelid in
        let elsel = "else" ^ labelid in
        let endl = "end" ^ labelid in
        let unconditional = "br label %" ^ endl ^ "\n" in
        let jmp = "br i1 " ^ reg ^ ", label %" ^ ifl ^ ", label %" ^ elsel ^ "\n" in
        let preg = gen () in
        let phi = preg ^ " = phi i32 [" ^ xname ^ ", %" ^ ifl ^ "], [" ^ yname ^ ", %" ^ elsel ^ "]\n" in
        let finally =
            cdef ^
            comp ^ jmp ^ ifl ^ ":\n" ^
            xdef ^ unconditional ^ elsel ^ ":\n" ^
            ydef ^ unconditional ^ endl ^ ":\n" ^
            phi in
        (finally, preg);




        (*
        let (cdef, cname) = compile gen cond in
        let (xdef, xname) = compile gen x in
        let (ydef, yname) = compile gen y in
        let sreg = gen () in
        let rreg = gen() in
        let compare = sreg ^ " = icmp ne i32 " ^ cname ^ ", 0\n" in
        let select = rreg ^ " = select i1 " ^ sreg ^ ", i32 " ^ xname ^ ", i32 " ^ yname ^ "\n" in
        let finally =
            cdef ^ xdef ^ ydef ^
            compare ^ select in
        (finally, rreg);
        *)

