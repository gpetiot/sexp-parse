%{
%}

%token EOF

%token LPAREN "("
%token RPAREN ")"

%token <string> SYMBOL "symbol"
%token <string> STRING "string"
%token <int> INT "1234"
%token <float> FLOAT "1.234"

%start sexp_list
%type <Sexp_intf.t list> sexp_list

%%

atom:
  | x = SYMBOL
    { Sexp_intf.Symbol x }
  | x = STRING
    { Sexp_intf.String x }
  | x = INT
    { Sexp_intf.Int x }
  | x = FLOAT
    { Sexp_intf.Float x }
;

sexp:
  | LPAREN s = sexp+ RPAREN
    { Sexp_intf.List s }
  | a = atom
    { Sexp_intf.Atom a }
;

sexp_list:
  | x = sexp* EOF
    { x }
;
