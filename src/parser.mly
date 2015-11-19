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

%token LAMBDA ARROW

%token IF THEN ELSE END

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
    | name = IDENT; ASSIGNMENT; e = exp                                     { Decl (name, e) }
    | name = IDENT; ASSIGNMENT; LAMBDA; LBRACE; args = separated_list(COMMA, IDENT); RBRACE; ARROW; e = exp   { Function (name, args, e) }

math:
    | LBRACE; m = math; RBRACE      { m }
    | v = IDENT                     { Var v }
    | i = NUMBER                    { Int i }
    | SUB; m = math                 { Arith (Sub, Int 0, m) }
    | m = math; o = op; n = math    { Arith (o, m, n) }
    | m = math; b = bop; n = math   { Comp (b, m, n) }
    | i = IDENT; LBRACE; el = separated_list(COMMA, exp); RBRACE            { Call (i, el) }
    | LCURLY; el = list(exp); RCURLY    { EList el }

    | IF; c = math; THEN; a = math; ELSE; b = math; END  { IfElse (c, a, b) }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }

%inline bop:
    | EQ    { Eq }
    | NE    { Ne }

    | LT    { Lt }
    | GT    { Gt }
    | LE    { Le }
    | GE    { Ge }
