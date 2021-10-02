%{
%}

%token EOF

%token LPAREN "("
%token RPAREN ")"

%token <string> ATOM "atom"

%start sexp_list
%type <Sexplib0.Sexp.t list> sexp_list

%%

sexp:
  | LPAREN s = sexp+ RPAREN
    { Sexplib0.Sexp.List s }
  | a = ATOM
    { Sexplib0.Sexp.Atom a }
;

sexp_list:
  | x = sexp* EOF
    { x }
;
