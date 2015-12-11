module String = Core.Std.String
open Syntax
open Llvm

exception LogicalError of string

let context = global_context ()
let mdl = create_module context "yip"
let builder = builder context
let integer_type = integer_type context 64

let global_scope = Scope.create builder

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
        Scope.local_add scope name alloca) merge

let rec compile scope = function
	| Decl (name, value) ->
        let value = compile scope value in
        let alloca = build_alloca integer_type name builder in
        ignore (build_store value alloca builder);
        (* YOU SHOULD CHECK IF THE VARIABLE EXISTS IN THE CURRENT SCOPE *)
        (match Scope.local_mem scope name with
            | true -> raise (LogicalError "variable already declared in current scope")
            | false -> ());
        Scope.local_add scope name alloca;
        value
    | Assign (name, value) ->
        let value = compile scope value in
        let mem_loc = Scope.find scope name in
        let store = build_store value mem_loc builder in
        value 
    | Var name ->
        let mem_loc = Scope.find scope name in
        build_load mem_loc name builder
    
    | External (name, args) ->
        let fun_scope = Scope.create ~parent:global_scope builder in
        let args_type = Array.make (List.length args) integer_type in
        let fun_sig = function_type integer_type args_type in

        let fun_def = declare_function name fun_sig mdl in
        ignore (create_args fun_scope args (params fun_def));
        fun_def

    | Function (name, args, body) ->
        let fun_scope = Scope.create ~parent:global_scope builder in
        let args_type = Array.make (List.length args) integer_type in
        let fun_sig = function_type integer_type args_type in

        let fun_def = declare_function name fun_sig mdl in
        let block = append_block context "" fun_def in
        ignore (position_at_end block builder);

        ignore (create_args fun_scope args (params fun_def));
        
        let ret_val = compile fun_scope body in
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
    | IfElse (cond, a, b) ->
        let alloca = build_alloca integer_type "ifelsevar" builder in
        let zero = const_int integer_type 0 in
        let cond = compile scope cond in
        let cond = build_icmp Icmp.Ne cond zero "ifelse" builder in

        let start_block = insertion_block builder in
        let func = block_parent start_block in

        let if_block = append_block context "if" func in
        position_at_end if_block builder;
        let a = compile scope a in
        ignore (build_store a alloca builder);
        let end_if_block = insertion_block builder in

        let else_block = append_block context "else" func in
        position_at_end else_block builder;
        let b = compile scope b in
        ignore (build_store b alloca builder);
        let end_else_block = insertion_block builder in

        let merge_block = append_block context "end" func in
        position_at_end merge_block builder;

        position_at_end start_block builder;

        ignore (build_cond_br cond if_block else_block builder);

        position_at_end end_if_block builder;
        ignore (build_br merge_block builder);

        position_at_end end_else_block builder;
        ignore (build_br merge_block builder);

        position_at_end merge_block builder;
        build_load alloca "ld" builder
    | Block body ->
        let parent_block = block_parent (insertion_block builder) in
        let alloca = build_alloca integer_type "res" builder in

        let body_block = append_block context "block" parent_block in
        ignore (build_br body_block builder);

        let end_block = insertion_block builder in
        let ended = append_block context "end" parent_block in
        
        let block_scope =
            Scope.create
                ~parent:scope
                ~body_block:body_block
                ~end_block:ended
                ~end_res:alloca
                builder in
        
        ignore (position_at_end body_block builder);
        let body = List.map (compile block_scope) body in
       
        let ret_val = List.hd (List.rev body) in
        ignore (build_br ended builder);
        ignore (position_at_end ended builder);
        ignore (build_store ret_val alloca builder);
        (*ret_val*)
        build_load alloca "bres" builder
    | Repeat ->
        (match scope.body_block with
            | None -> raise (LogicalError "statement not in a block")
            | Some b ->
                ignore (build_br b builder);
                const_int integer_type 0)
    | Return value ->
        let value = compile scope value in
        let mem_loc = match scope.end_res with
            | None -> raise (LogicalError "statement not in a block")
            | Some b -> b in
        (match scope.end_block with
            | None -> raise (LogicalError "statement not in a block")
            | Some b ->
                ignore (build_store value mem_loc builder);
                ignore (build_br b builder);
                value)
