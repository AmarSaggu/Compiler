%{
    open Syntax
%}

%token <int> NUMBER
%token <bool> BOOLEAN
%token <string> IDENT STRING

%token LBRACE RBRACE
%token LCURLY RCURLY

%token ADD SUB
%token MUL DIV

%token LAMBDA ARROW

%token COMMA

%token ASSIGNMENT

%token EQUALITY INEQUALITY

%token TRUE FALSE

%token EOF


%right ARROW

%left ADD SUB
%left MUL DIV

%start <Syntax.ast list> top

%%

top:
    | vl = list(var); EOF   { vl }

var:
    | v = IDENT; ASSIGNMENT; LAMBDA; args = list(IDENT); ARROW; e = exp { VarDecl(v, Function (v, args, e)) }
    | v = IDENT; ASSIGNMENT; e = exp    { VarDecl (v, e) }

exp:
    | LBRACE; e = exp; RBRACE   { e }
    
    | b = BOOLEAN   { Boolean b }
    | i = NUMBER    { Integer i }
    | s = STRING    { String s }
    | v = IDENT     { Variable v }
    
    | SUB; e = exp              { Arithmetic (Sub, Integer 0, e) }
    | e = exp; o = op; f = exp  { Arithmetic (o, e, f) }
    
    | TRUE  { Integer 1 }
    | FALSE { Integer 0 }
    
    | f = func                  { f }

    | LCURLY; el = list(var); RCURLY    { EList el }

func:
    | i = IDENT; LBRACE; el = separated_list(COMMA, exp); RBRACE    { Execution (i, el) }

%inline op:
    | ADD   { Add }
    | SUB   { Sub }
    | MUL   { Mul }
    | DIV   { Div }
