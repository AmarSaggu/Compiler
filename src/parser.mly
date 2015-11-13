%{
    open Syntax
%}

%token <int> NUMBER
%token <string> IDENT

%token LBRACE RBRACE
%token LCURLY RCURLY

%token ADD SUB
%token MUL DIV

%token LAMBDA ARROW

%token IF THEN ELSE

%token COMMA

%token ASSIGNMENT

%token EOF


%right ARROW

%left ADD SUB
%left MUL DIV

%start <Syntax.ast list> top

%%

top:
    | vl = list(exp); EOF   { vl }

exp:
    | m = math {m}
    | d = decl { d }

%inline decl:
    | name = IDENT; ASSIGNMENT; e = exp                                     { Decl (name, e) }
    | name = IDENT; ASSIGNMENT; LAMBDA; args = list(IDENT); ARROW; e = exp  { Function (name, args, e) }

math:
    | LBRACE; m = math; RBRACE      { m }
    | v = IDENT                     { Variable v }
    | i = NUMBER                    { Integer i }
    | SUB; m = math                 { Arithmetic (Sub, Integer 0, m) }
    | m = math; o = op; n = math    { Arithmetic (o, m, n) }
    | i = IDENT; LBRACE; el = separated_list(COMMA, exp); RBRACE            { Call (i, el) }
    | LCURLY; el = list(exp); RCURLY    { EList el }

    | IF; c = math; THEN; a = math; ELSE; b = math  { IfElse (c, a, b) }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }
