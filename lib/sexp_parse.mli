module Sexp = Sexp

val of_lexbuf : Lexing.lexbuf -> (Sexp.t list, [ `Msg of string ]) result

val of_string : string -> (Sexp.t list, [ `Msg of string ]) result
