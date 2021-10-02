module Typed = struct
  include Sexp

  let of_lexbuf lx =
    match Sexp_parser.sexp_list Sexp_lexer.token lx with
    | exception Sexp_lexer.Error (err, loc) ->
        Error
          (`Msg
            (Format.asprintf "%a: %a" Location.print_loc loc Sexp_lexer.pp_error
               err))
    | exception Sexp_parser.Error ->
        let pos = lx.lex_curr_p in
        Error
          (`Msg
            (Format.asprintf "Line:%d Column:%d: syntax error@." pos.pos_lnum
               (pos.pos_cnum - pos.pos_bol)))
    | x -> Ok x

  let of_string s = of_lexbuf (Lexing.from_string s)
end
