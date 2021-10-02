module Make (Sexp : sig
  type t
end) (Parser : sig
  type token

  exception Error

  val sexp_list : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Sexp.t list
end) (Lexer : sig
  type error

  exception Error of error * Location.t

  val pp_error : Format.formatter -> error -> unit

  val token : Lexing.lexbuf -> Parser.token
end) =
struct
  let of_lexbuf lx =
    match Parser.sexp_list Lexer.token lx with
    | exception Lexer.Error (err, loc) ->
        Error
          (`Msg
            (Format.asprintf "%a: %a" Location.print_loc loc Lexer.pp_error err))
    | exception Parser.Error ->
        let pos = lx.lex_curr_p in
        Error
          (`Msg
            (Format.asprintf "Line:%d Column:%d: syntax error@." pos.pos_lnum
               (pos.pos_cnum - pos.pos_bol)))
    | x -> Ok x

  let of_string s = of_lexbuf (Lexing.from_string s)
end

module Typed = struct
  include Sexp
  include Make (Sexp) (Sexp_parser) (Sexp_lexer)
end

module Untyped = Make (Sexplib0.Sexp) (Sexp0_parser) (Sexp0_lexer)
