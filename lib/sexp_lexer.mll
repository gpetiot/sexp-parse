{
open Sexp_parser

type error =
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Unterminated_string
  | Invalid_literal of string

exception Error of error * Location.t

let pp_error fs = function
  | Illegal_character c ->
    Format.fprintf fs "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape (s, explanation) ->
    Format.fprintf fs
      "Illegal backslash escape in string or character (%s)%t" s
      (fun ppf -> match explanation with
         | None -> ()
         | Some expl -> Format.fprintf ppf ": %s" expl)
  | Unterminated_string ->
     Format.fprintf fs "String literal not terminated"
  | Invalid_literal s ->
     Format.fprintf fs "Invalid literal %s" s

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer
let store_string_char c = Buffer.add_char string_buffer c
let store_string_utf_8_uchar u = Buffer.add_utf_8_uchar string_buffer u
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)
let is_in_string = ref false
let string_start_loc = ref Location.none

(* Escaped chars are interpreted in strings. *)
let store_escaped_char _lexbuf c = store_string_char c

let store_escaped_uchar _lexbuf u = store_string_utf_8_uchar u

(* to translate escape sequences *)

let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let illegal_escape lexbuf reason =
  let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
  raise (Error (error, Location.curr lexbuf))

let char_for_decimal_code lexbuf i =
  let c = num_value lexbuf ~base:10 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    illegal_escape lexbuf
      (Printf.sprintf
         "%d is outside the range of legal characters (0-255)." c)
  else Char.chr c

let uchar_for_uchar_escape lexbuf =
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true ->
      illegal_escape lexbuf
        "too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = num_value lexbuf ~base:16 ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
      illegal_escape lexbuf
        (Printf.sprintf "%X is not a Unicode scalar value" cp)

let char_for_octal_code lexbuf i =
  let c = num_value lexbuf ~base:8 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    illegal_escape lexbuf
      (Printf.sprintf
         "o%o (=%d) is outside the range of legal characters (0-255)." c c)
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  Char.chr (num_value lexbuf ~base:16 ~first:i ~last:(i+1))

let wrap_string_lexer f (lexbuf : Lexing.lexbuf) =
  let loc_start = lexbuf.lex_curr_p in
  reset_string_buffer();
  is_in_string := true;
  let string_start = lexbuf.lex_start_p in
  string_start_loc := Location.curr lexbuf;
  let loc_end = f lexbuf in
  is_in_string := false;
  lexbuf.lex_start_p <- string_start;
  let loc = Location.{loc_ghost= false; loc_start; loc_end} in
  get_stored_string (), loc

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

let update_loc (lexbuf : Lexing.lexbuf) file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

let escaped_newlines = ref false

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let identchar_latin1 =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9'
   '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '#']

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let comment =
  ';' [^ '\013' '\010']* newline

rule token = parse
  | ('\\' as bs) newline {
      if not !escaped_newlines then error lexbuf (Illegal_character bs);
      update_loc lexbuf None 1 false 0;
      token lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf }
  | blank +
      { token lexbuf }
  | comment
      { token lexbuf }
  | eof { EOF }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "\""
      { let s, _ = wrap_string_lexer string lexbuf in
        STRING s }
  | int_literal as lit { INT (int_of_string lit) }
  | float_literal | hex_float_literal as lit
      { FLOAT (float_of_string lit) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as invalid
      { error lexbuf (Invalid_literal invalid) }
  | identchar_latin1+ as name
      { SYMBOL name }
  | (_ as illegal_char)
      { error lexbuf (Illegal_character illegal_char) }

and string = parse
    '\"'
      { lexbuf.lex_start_p }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c)
      { store_escaped_char lexbuf (char_for_backslash c);
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_escaped_char lexbuf (char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7']
      { store_escaped_char lexbuf (char_for_octal_code lexbuf 2);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_escaped_char lexbuf (char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
        { store_escaped_uchar lexbuf (uchar_for_uchar_escape lexbuf);
          string lexbuf }
  | '\\' _
      { begin
(*  Should be an error, but we are very lax.
          error lexbuf (Illegal_escape (Lexing.lexeme lexbuf, None))
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
        end;
        store_lexeme lexbuf;
        string lexbuf
      }
  | newline
      { Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      }
  | eof
      { is_in_string := false;
        error_loc !string_start_loc Unterminated_string }
  | (_ as c)
      { store_string_char c;
        string lexbuf }

{
}
