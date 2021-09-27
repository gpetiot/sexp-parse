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
%type <Sexp.t list> sexp_list

%%

atom:
  | x = SYMBOL
    { Sexp.Symbol x }
  | x = STRING
    { Sexp.String x }
  | x = INT
    { Sexp.Int x }
  | x = FLOAT
    { Sexp.Float x }
;

sexp:
  | LPAREN s = sexp+ RPAREN
    { Sexp.List s }
  | a = atom
    { Sexp.Atom a }
;

sexp_list:
  | x = sexp* EOF
    { x }
;
