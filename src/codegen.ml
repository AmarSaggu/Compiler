module String = Core.Std.String
open Syntax
open Llvm

exception LogicalError of string

let context = global_context ()
let mdl = create_module context "yip"
let builder = builder context
let integer_type = integer_type context 64

let global_scope = Scope.create ()

let op_to_function = function
    | Add -> build_add
    | Sub -> build_sub
    | Mul -> build_mul
    | Div -> build_sdiv

let comp_to_function = function
    | Eq -> Icmp.Eq
    | Ne -> Icmp.Ne
    | Gt -> Icmp.Sgt
    | Ge -> Icmp.Sge
    | Lt -> Icmp.Slt
    | Le -> Icmp.Sle

let create_args scope args params =
    let params = Array.to_list params in
    let merge = List.combine args params in
    List.map (fun (name, value) ->
        let alloca = build_alloca integer_type name builder in
        ignore (build_store value alloca builder);
        Scope.add scope name alloca) merge

let rec compile scope = function
	| Decl (name, value) ->
        let value = compile scope value in
        let alloca = build_alloca integer_type name builder in
        ignore (build_store value alloca builder);
        Scope.add scope name alloca;
        alloca
    | Var name ->
        let mem_loc = Scope.find scope name in
        build_load mem_loc name builder
    | Function (name, args, body) ->
        let fun_scope = Scope.create ~parent:global_scope () in
        let args_type = Array.make (List.length args) integer_type in
        let fun_sig = function_type integer_type args_type in

        let fun_def = declare_function name fun_sig mdl in
        let block = append_block context "" fun_def in
        ignore (position_at_end block builder);

        ignore (create_args fun_scope args (params fun_def));
        
        let total = List.map (compile fun_scope) body in
        let ret_val = List.hd (List.rev total) in
        ignore (build_ret ret_val builder);
        fun_def
    | Call (name, args) ->
        let name = match lookup_function name mdl with
            | Some p -> p
            | None -> raise (LogicalError "function does not exist")
        in
        let params = params name in
        let args = Array.of_list (List.map (compile scope) args) in
        build_call name args "call" builder
    | Int i -> const_int integer_type i
    | Arith (op, a, b) ->
        let opfun = op_to_function op in
        let a = compile scope a in
        let b = compile scope b in
        opfun a b "arith" builder
    | Comp (comp, a, b) ->
        let compfun = comp_to_function comp in
        let a = compile scope a in
        let b = compile scope b in
        let res = build_icmp compfun a b "cmp" builder in
        build_zext res integer_type "zext" builder

