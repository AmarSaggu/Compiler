open Core.Std
open Syntax

(*let indent str = String.tr ~target:"\n" ~replacement:"\n\t"*)

let indent str =
    "\t" ^
    (String.split str ~on:'\n'
    |> String.concat ~sep:"\n\t")

let main_fun (ret, main) =
    "define i32 @main()\n{\n" ^ (indent (main ^ "\nret i32 ")) ^ ret ^ "\n}"

let assign_reg =
    let reg = ref 0 in
    fun () ->
        incr reg;
        "%" ^ (string_of_int !reg)

type asm =
    | I of int
    | A of asm * asm
   
let rec comp = function
    | I i -> (string_of_int i, "")
    | A (x, y) ->
        let (cx, fx) = comp x in
        let (cy, fy) = comp y in
        let decl = assign_reg () in
        let def = decl ^ " = add i32 " ^ cx ^ ", " ^ cy ^ "\n" in
        (decl, fx ^ fy ^ def)

let op_to_opcode = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "sdiv"

let rec syn = function
    | Integer i -> (string_of_int i, "")
    | Arithmetic (op, x, y) ->
        let (xdecl, xdef) = syn x in
        let (ydecl, ydef) = syn y in
        let decl = assign_reg () in
        let op' = op_to_opcode op in
        let def = decl ^ " = " ^ op' ^ " i32 " ^ xdecl ^ ", " ^ ydecl ^ "\n" in
        (decl, xdef ^ ydef ^ def)
    | _ ->
        let err = "error: operation not supported" in
        (err, err)

let syn_ast = function
    | VarDecl (str, expr) -> syn expr

        (*
let code = A (A ((I 4), (I 8)), I 9)
let res = comp code;;

print_endline (main_fun (snd res) (fst res))*)
