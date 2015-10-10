%token <int> INT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token LEFT_BRACE
%token RIGHT_BRACE
%token ASSIGNMENT
%token <string> VARIABLE
%token COMMA
%token EOF
%left PLUS MINUS
%left TIMES DIVIDE
%start <(string * int) list> top
%%
top:
    | vl = separated_list (COMMA, var); EOF  { vl }

var:
    | v = VARIABLE; ASSIGNMENT; e = exp { (v,e) }

exp:
    | i = INT                           { i }
    | e = exp; PLUS; f = exp            { e + f }
    | e = exp; MINUS; f = exp           { e - f }
    | e = exp; TIMES; f = exp           { e * f }
    | e = exp; DIVIDE; f = exp          { e / f }
    | LEFT_BRACE; e = exp; RIGHT_BRACE  { (e) }
    | LEFT_BRACE; v = var; RIGHT_BRACE  { snd v }
