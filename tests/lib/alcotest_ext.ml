let error_msg =
  let pp fs (`Msg x) = Format.fprintf fs "(`Msg %S)" x in
  let eq (`Msg x) (`Msg y) = String.equal x y in
  Alcotest.testable pp eq

let result_msg testable = Alcotest.result testable error_msg

let eq_atom (x : Sexp_parse.Sexp.atom) (y : Sexp_parse.Sexp.atom) =
  match (x, y) with
  | Symbol x, Symbol y -> String.equal x y
  | String x, String y -> String.equal x y
  | Int x, Int y -> Int.equal x y
  | Float x, Float y -> Float.equal x y
  | _ -> false

let rec eq_sexp (x : Sexp_parse.Sexp.t) (y : Sexp_parse.Sexp.t) =
  match (x, y) with
  | Atom x, Atom y -> eq_atom x y
  | List x, List y -> eq_sexp_list x y
  | _ -> false

and eq_sexp_list x y =
  match (x, y) with
  | [], [] -> true
  | a :: b, c :: d -> eq_sexp a c && eq_sexp_list b d
  | _ -> false

let sexp =
  let pp = Sexp_parse.Sexp.dump in
  let eq = eq_sexp in
  Alcotest.testable pp eq
