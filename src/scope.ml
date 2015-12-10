open Llvm

exception ScopeError of string 

type scope = {
    variables: (string, llvalue) Hashtbl.t;
    body_block: llbasicblock option;
    end_block: llbasicblock option;
    end_res: llvalue option;
    parent: scope option;
};;

let create ?parent ?body_block ?end_block ?end_res builder = {
    variables = Hashtbl.create 10;
    body_block = body_block;
    end_block = end_block;
    end_res = end_res;

    parent = parent;
}

let local_add scope var = Hashtbl.add scope.variables var

let local_mem scope var = 
    Hashtbl.mem scope.variables var 

let rec mem scope var =
    let res = local_mem scope var in
    match res with
        | true -> true
        | false ->
            match scope.parent with
                | Some parent -> mem parent var
                | None -> false

let local_find scope var = 
    Hashtbl.find scope.variables var 

let rec find scope var = 
    let res = local_mem scope var in
    match res with
        | true -> local_find scope var
        | false ->
            match scope.parent with
                | Some parent -> find parent var
                | None -> raise (ScopeError ("'" ^ var ^ "' does not exist"))
