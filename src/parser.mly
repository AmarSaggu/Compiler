%{
    open Syntax
%}

%token <int> NUMBER
%token <string> IDENTIFIER STRING

%token ADD SUB
%token MUL DIV

%token LBRACE RBRACE

%token EQUALS
%token EOF

%token LAMBDA ARROW

%right ARROW

%left ADD SUB
%left MUL DIV


%start <Syntax.ast list> top

%%

top:
    | vl = list(var); EOF  { vl }

var:
    | v = IDENTIFIER; EQUALS; e = exp       { VarDecl (v, e) }

exp:
    | f = func                  { f }
    | LBRACE; e = exp; RBRACE   { e }
    | v = IDENTIFIER            { Variable v }
    | i = NUMBER                { Integer i }
    | s = STRING                { String s }
    | SUB; e = exp              { Arithmetic (Sub, Integer 0, e) }
    | e = exp; ADD; f = exp     { Arithmetic (Add, e, f) }
    | e = exp; SUB; f = exp     { Arithmetic (Sub, e, f) }
    | e = exp; MUL; f = exp     { Arithmetic (Mul, e, f) }
    | e = exp; DIV; f = exp     { Arithmetic (Div, e, f) }

func:
    | LAMBDA; nl = list(IDENTIFIER); ARROW; e = exp     { Function (nl, e) }
