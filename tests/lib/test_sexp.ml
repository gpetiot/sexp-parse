let test_of_lexbuf =
  let _make_test ~name ~lexbuf ~expected =
    let name = "of_lexbuf " ^ name in
    let test_fun () =
      Alcotest.(check (Alcotest_ext.result_msg (list Alcotest_ext.sexp)))
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
      Alcotest.(check (Alcotest_ext.result_msg (list Alcotest_ext.sexp)))
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
