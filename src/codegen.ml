module String = Core.Std.String
open Syntax

let rec last = function
    | [] -> failwith "empty list contains no elements"
    | [hd] -> hd
    | hd :: tl -> last tl

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

let comp_to_instr = function
    | Eq -> "eq"
    | Ne -> "ne"

    | Lt -> "slt"
    | Gt -> "sgt"
    | Le -> "sle"
    | Ge -> "sge"

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
    let return = "ret i32 " ^ (if name <> "main" then (snd body) else "0") in
    let print = if name <> "main" then ""
        else "call i32 (i8*, ...) @printf(i8 *getelementptr ([4 x i8], [4 x i8]* @_str, i64 0, i64 0), i32 " ^ (snd body) ^ ")\n" in
    let print_str = if name <> "main" then ""
        else "@_str = constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n" in
    let declare = if name <> "main" then ""
        else "\ndeclare i32 @printf(i8*, ...)\n" in
    "\n" ^ print_str ^ header ^ "\n{\n" ^ (indent ((fst body) ^ "\n" ^ print ^ return)) ^ "\n}\n" ^ declare

and arithmetic gen label = function
    | Int i -> ("", string_of_int i)
    | Arith (op, x, y) ->
        let (xdef, xname) = arithmetic gen label x in
        let (ydef, yname) = arithmetic gen label y in
        let op_str = op_to_opcode op in
        let reg = gen () in
        (xdef ^ ydef ^ reg ^ " = " ^ op_str ^ " i32 " ^ xname ^ ", " ^ yname ^ "\n", reg)
    | a -> compile gen label a

and compile gen label = function
    | Int i ->
        let i_str = string_of_int i in
        let reg = gen () in
        (reg ^ " = add i32 " ^ i_str ^ ", 0\n", reg)
    | Decl (name, exp) ->
        let (def, reg) = compile gen label exp in
        (def ^ "%" ^ name ^ " = add i32 " ^ reg ^ ", 0\n", "%" ^ name)
    | Var v ->
        ("", "%" ^ v)

    | Function (name, args, body) ->
        let args' = List.map ((^) "%") args in
        let body' =
            let comp = List.map (compile (create_reg_generator "%") (create_reg_generator "")) body in
            let name = snd (last comp) in
            let def = 
                List.map fst comp
                |> Bytes.concat "" in
            (def, name) in
        (define_fun name args' body', "")
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

    | Arith (op, x, y) ->
        arithmetic gen label (Arith (op, x, y))
    | Comp (bop, x, y) ->
        let (xdef, xname) = compile gen label x in
        let (ydef, yname) = compile gen label y in
        let reg = gen () in
        let big = gen () in
        let instr = comp_to_instr bop in
        let comp = reg ^ " = icmp " ^ instr ^ " i32 " ^ xname ^ ", " ^ yname ^ "\n" in
        let zext = big ^ " = zext i1 " ^ reg ^ "to i32\n" in
        (xdef ^ ydef ^ comp ^ zext, big)

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
