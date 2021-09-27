let error_msg =
  let pp fs (`Msg x) = Format.fprintf fs "(`Msg %S)" x in
  let eq (`Msg x) (`Msg y) = String.equal x y in
  Alcotest.testable pp eq

let result_msg testable = Alcotest.result testable error_msg

let eq_atom (x : Sexp_parse.atom) (y : Sexp_parse.atom) =
  match (x, y) with
  | Symbol x, Symbol y -> String.equal x y
  | String x, String y -> String.equal x y
  | Int x, Int y -> Int.equal x y
  | Float x, Float y -> Float.equal x y
  | _ -> false

let rec eq_sexp (x : Sexp_parse.t) (y : Sexp_parse.t) =
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
  let pp = Sexp_parse.pp in
  let eq = eq_sexp in
  Alcotest.testable pp eq

let test_of_lexbuf =
  let _make_test ~name ~lexbuf ~expected =
    let name = "of_lexbuf " ^ name in
    let test_fun () =
      Alcotest.(check (result_msg (list sexp)))
        name expected
        (Sexp_parse.of_lexbuf lexbuf)
    in
    (name, `Quick, test_fun)
  in
  []

let test_of_string =
  let make_test ~name ~input ~expected =
    let name = "of_string " ^ name in
    let test_fun () =
      Alcotest.(check (result_msg (list sexp)))
        name expected
        (Sexp_parse.of_string input)
    in
    (name, `Quick, test_fun)
  in
  [
    make_test ~name:"rosettacode example"
      ~input:
        {|
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
|}
      ~expected:(Ok []);
  ]

let suite = ("Sexp", test_of_lexbuf @ test_of_string)
