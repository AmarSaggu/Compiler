%{
    open Syntax
%}

%token <int> NUMBER
%token <string> IDENT

%token LBRACE RBRACE

%token ADD SUB
%token MUL DIV

%token EQ NE
%token LE GE LT GT

%token LAMBDA END

%token IF THEN ELSE 

%token COMMA

%token ASSIGNMENT

%token EOF


%left EQ NE
%left LE GE LT GT

%left ADD SUB
%left MUL DIV

%start <Syntax.ast list> top

%%

top:
    | vl = list(exp); EOF   { vl }

exp:
    | d = decl { d }
    | m = math { m }

%inline decl:
    | name = IDENT; ASSIGNMENT; e = exp { Decl (name, e) }
    | name = IDENT; ASSIGNMENT; LAMBDA; LBRACE; args = separated_list(COMMA, IDENT); RBRACE; body = exp+; END   { Function (name, args, body) }

math:
    | LBRACE; m = math; RBRACE      { m }
    | v = IDENT                     { Var v }
    | i = NUMBER                    { Int i }
    | SUB; m = math                 { Arith (Sub, Int 0, m) }
    | m = math; o = op; n = math    { Arith (o, m, n) }
    | m = math; c = cop; n = math   { Comp (c, m, n) }
    | i = IDENT; LBRACE; el = separated_list(COMMA, exp); RBRACE            { Call (i, el) }

    | IF; c = math; THEN; a = math; ELSE; b = math; END  { IfElse (c, a, b) }

    | LAMBDA; LBRACE; args = separated_list(COMMA, IDENT); RBRACE; body = exp+; END    { Function ("yee", args, body) }

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
