include module type of Sexp_intf

val pp : Format.formatter -> t -> unit

val of_lexbuf : Lexing.lexbuf -> (t list, [`Msg of string]) result

val of_string : string -> (t list, [`Msg of string]) result
