let error_msg =
  let pp fs (`Msg x) = Format.fprintf fs "(`Msg %S)" x in
  let eq (`Msg x) (`Msg y) = String.equal x y in
  Alcotest.testable pp eq

let result_msg testable = Alcotest.result testable error_msg

let sexp =
  let pp = Sexp_parse.Typed.dump in
  let eq = Sexp_parse.Typed.equal in
  Alcotest.testable pp eq

let sexp0 =
  let pp = Sexplib0.Sexp.pp_hum in
  let eq = Sexplib0.Sexp.equal in
  Alcotest.testable pp eq
