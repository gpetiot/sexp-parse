val result_msg :
  'a Alcotest.testable -> ('a, [ `Msg of string ]) result Alcotest.testable

val sexp : Sexp_parse.Sexp.t Alcotest.testable
