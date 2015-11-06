%{
    open Syntax
%}

%token <int> NUMBER
%token <string> IDENT STRING

%token ADD SUB
%token MUL DIV

%token LBRACE RBRACE

%token COMMA

%token EQUALS
%token EOF

%token LAMBDA ARROW

%right ARROW

%left ADD SUB
%left MUL DIV

%start <Syntax.ast list> top

%%

top:
    | vl = list(var); EOF   { vl }

var:
    | v = IDENT; EQUALS; e = exp    { VarDecl (v, e) }

exp:
    | i = NUMBER                    { Integer i }
    | s = STRING                    { String s }
    | v = IDENT                     { Variable v }
    | SUB; e = exp                  { Arithmetic (Sub, Integer 0, e) }
    | e = exp; o = op; f = exp      { Arithmetic (o, e, f) }
    | LBRACE; e = exp; RBRACE       { e }
    | f = func                      { f }

func:
    | LAMBDA; args = list(IDENT); ARROW; e = exp           { Function (args, e) }
    | i = IDENT; LBRACE; el = separated_list(COMMA, exp); RBRACE    { Execution (i, el) }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }

