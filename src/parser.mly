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

%right IDENTIFIER

%start <Syntax.ast list> top

%%

top:
    | vl = list(var); EOF { vl }

var:
    | v = IDENTIFIER; EQUALS; e = exp2 { VarDecl (v, e) }

exp2:
    | i = IDENTIFIER; args = nonempty_list(args) { Execution (i, args) }
    | e = exp { e }

exp:
    | v = IDENTIFIER            { Variable v }
    | f = func                  { f }
    | b = boolean               { Boolean b }
    | i = NUMBER                { Integer i }
    | s = STRING                { String s }
    | SUB; e = exp              { Arithmetic (Sub, Integer 0, e) }
    | e = exp; o = op; f = exp  { Arithmetic (o, e, f) }
    | LBRACE; e = exp; RBRACE   { e }

%inline func:
    | LAMBDA; args = nonempty_list(IDENTIFIER); ARROW; e = exp { Function (args, e) }

%inline boolean:
    | TRUE  { true }
    | FALSE { false }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }
