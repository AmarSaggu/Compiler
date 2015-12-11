%{
    open Syntax

    let while_to_ast cond body =
        let cond_name = "_cond_" in
        Block [
            Decl (cond_name, cond);
            IfElse (Var cond_name, Block [body], Int 0);
            IfElse (Var cond_name, Repeat, Int 0);
        ]
    
    let for_to_ast a b c body =
        Block [
            a;
            while_to_ast b (Block [body; c]);
        ]
%}

%token <int> NUMBER
%token <string> IDENT

%token LBRACE RBRACE
%token LCURLY RCURLY

%token ADD SUB
%token MUL DIV

%token EQ NE
%token LE GE LT GT

%token LAMBDA EXTERNAL

%token LET

%token REPEAT RETURN

%token WHILE FOR

%token IF ELSE 

%token COMMA

%token ASSIGNMENT

%token EOF


%right ELSE

%left EQ NE
%left LE GE LT GT

%left ADD SUB
%left MUL DIV

%start <Syntax.ast list> top

%%

top:
    | vl = list(func); EOF   { vl }

func:
    | LAMBDA; name = IDENT; LBRACE; args = separated_list(COMMA, IDENT); RBRACE; body = math { Function (name, args, body) }
    | EXTERNAL; name = IDENT; LBRACE; args = separated_list(COMMA, IDENT); RBRACE { External (name, args) }

math:
    | LBRACE; m = math; RBRACE      { m }
    | v = IDENT                     { Var v }
    | i = NUMBER                    { Int i }
    | m = math; o = op; n = math    { Arith (o, m, n) }
    | SUB; m = math                 { Arith (Sub, Int 0, m) }
    | m = math; c = cop; n = math   { Comp (c, m, n) }
    | i = IDENT; LBRACE; el = separated_list(COMMA, math); RBRACE            { Call (i, el) }

    | LET; name = IDENT; ASSIGNMENT; e = math { Decl (name, e) }
    | name = IDENT; ASSIGNMENT; e = math { Assign (name, e) }
    | LCURLY; b = nonempty_list(math); RCURLY   { Block b }
    | IF; c = math; a = math; ELSE; b = math { IfElse (c, a, b) }
    | REPEAT { Repeat }
    | RETURN; m = math { Return m }

    | WHILE; cond = math; body = math   { while_to_ast cond body }
    | FOR; a = math; COMMA; b = math; COMMA; c = math; body = math { for_to_ast a b c body }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }

%inline cop:
    | EQ    { Eq }
    | NE    { Ne }

    | LT    { Lt }
    | GT    { Gt }
    | LE    { Le }
    | GE    { Ge }
