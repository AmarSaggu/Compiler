%{
    open Syntax
%}

%token <int> INT
%token <string> STRING
%token <string> VAR

%token PLUS
%token MINUS
%token TIMES
%token DIVIDE

%token LEFT_BRACE
%token RIGHT_BRACE

%token EQUALS
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE

%start <Syntax.ast list> top

%%

top:
    | vl = list(var); EOF  { vl }

var:
    | v = VAR; EQUALS; e = exp      { VarDecl (v, Int e) }
    | v = VAR; EQUALS; s = STRING   { VarDecl (v, Str s) }

exp:
    | i = INT                           { IntExp i }
    | v = VAR                           { VarExp v }
    | e = exp; PLUS; f = exp            { Exp (Add, e, f) }
    | e = exp; MINUS; f = exp           { Exp (Sub, e, f) }
    | e = exp; TIMES; f = exp           { Exp (Mul, e, f) }
    | e = exp; DIVIDE; f = exp          { Exp (Div, e, f) }
    | LEFT_BRACE; e = exp; RIGHT_BRACE  { e }
