module Typed : sig
  include module type of Sexp

  val of_lexbuf : Lexing.lexbuf -> (Sexp.t list, [ `Msg of string ]) result

  val of_string : string -> (Sexp.t list, [ `Msg of string ]) result
end

module Untyped : sig
  val of_lexbuf :
    Lexing.lexbuf -> (Sexplib0.Sexp.t list, [ `Msg of string ]) result

  val of_string : string -> (Sexplib0.Sexp.t list, [ `Msg of string ]) result
end
