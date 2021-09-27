module Sexp = Sexp

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
