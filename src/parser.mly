%{
    open Syntax
%}

%token <int> NUMBER
%token <string> IDENTIFIER STRING

%token TRUE FALSE

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
    | b = boolean               { Boolean b }
    | v = IDENTIFIER            { Variable v }
    | i = NUMBER                { Integer i }
    | s = STRING                { String s }
    | SUB; e = exp              { Arithmetic (Sub, Integer 0, e) }
    | e = exp; o = op; f = exp  { Arithmetic (o, e, f) }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }

%inline boolean:
    | TRUE  { true }
    | FALSE { false }

func:
    | LAMBDA; args = list(IDENTIFIER); ARROW; e = exp     { Function (args, e) }
