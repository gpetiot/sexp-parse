include Sexp_intf

let pp_atom fs = function
  | Symbol x -> Format.fprintf fs "%s" x
  | String x -> Format.fprintf fs "%S" x
  | Int x -> Format.fprintf fs "%i" x
  | Float x -> Format.fprintf fs "%f" x

let rec pp fs = function
  | List x ->
    Format.pp_print_list pp fs x
      ~pp_sep:(fun fs () -> Format.fprintf fs "@ ")
  | Atom x ->
    pp_atom fs x

let of_lexbuf lx =
  match Parser.sexp_list Lexer.token lx with
  | exception (Lexer.Error (err, loc)) ->
    Error (`Msg (Format.asprintf "%a%a" Location.print_loc loc Lexer.pp_error err))
  | x -> Ok x

let of_string s = of_lexbuf (Lexing.from_string s)
