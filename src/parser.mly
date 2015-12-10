%{
    open Syntax
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

%token REPEAT
%token RETURN

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
