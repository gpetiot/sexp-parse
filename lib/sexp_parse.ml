include Sexp

let of_lexbuf lx =
  match Parser.sexp_list Lexer.token lx with
  | exception Lexer.Error (err, loc) ->
      Error
        (`Msg
          (Format.asprintf "%a%a" Location.print_loc loc Lexer.pp_error err))
  | x -> Ok x

let of_string s = of_lexbuf (Lexing.from_string s)
