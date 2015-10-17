%{
    open Syntax
%}

%token <int> INT
%token <string> STRING VAR

%token ADD SUB
%token MUL DIV

%token LBRACE RBRACE

%token EQUALS
%token EOF

%left ADD SUB
%left MUL DIV

%start <Syntax.ast list> top

%%

top:
    | vl = list(var); EOF  { vl }

var:
    | v = VAR; EQUALS; e = exp      { VarDecl (v, Int e) }
    | v = VAR; EQUALS; s = STRING   { VarDecl (v, Str s) }

exp:
    | i = INT                   { IntExp i }
    | v = VAR                   { VarExp v }
    | LBRACE; e = exp; RBRACE   { e }
    | e = exp; ADD; f = exp     { Exp (Add, e, f) }
    | e = exp; SUB; f = exp     { Exp (Sub, e, f) }
    | e = exp; MUL; f = exp     { Exp (Mul, e, f) }
    | e = exp; DIV; f = exp     { Exp (Div, e, f) }
