%{
    open Syntax
%}

%token <int> NUMBER
%token <string> IDENTIFIER STRING

%token TRUE FALSE

%token ADD SUB
%token MUL DIV

%token SEMICOLON

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
    | v = IDENTIFIER; EQUALS; e = exp { VarDecl (v, e) }
    | v = IDENTIFIER; EQUALS; e = yee { VarDecl (v, e) }

exp:
    | f = func                  { f }
    | i = NUMBER                { Integer i }
    | s = STRING                { String s }
    | LBRACE; SUB; e = exp; RBRACE              { Arithmetic (Sub, Integer 0, e) }
    | e = exp; o = op; f = exp  { Arithmetic (o, e, f) }
    | LBRACE; e = exp; RBRACE   { e }
    | v = IDENTIFIER            { Variable v }
    | i = IDENTIFIER; el = nonempty_list(exp) { Execution (i, el) }

yee:
    | e = exp { e }

func:
    | LAMBDA; args = nonempty_list(IDENTIFIER); ARROW; e = exp { Function (args, e) }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }

