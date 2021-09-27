let test_of_lexbuf =
  let make_test ~file ~expected =
    let name = "of_lexbuf " ^ file in
    let test_fun () =
      let ic = open_in file in
      let lx = Lexing.from_channel ic in
      close_in ic ;
      Alcotest.(check (Alcotest_ext.result_msg (list Alcotest_ext.sexp)))
        name expected
        (Sexp_parse.of_lexbuf lx)
    in
    (name, `Quick, test_fun)
  in
  [ make_test ~file:"ocamlformat-dune-project" ~expected:(Ok []) ]

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
      ~expected:
        (Ok
           Sexp_parse.Sexp.
             [
               List
                 [
                   List
                     [
                       Atom (Symbol "data");
                       Atom (String {|quoted data|});
                       Atom (Int 123);
                       Atom (Float 4.5);
                     ];
                   List
                     [
                       Atom (Symbol "data");
                       List
                         [
                           Atom (Symbol "!@#");
                           List [ Atom (Float 4.5) ];
                           Atom (String "(more");
                           Atom (String "data)");
                         ];
                     ];
                 ];
             ]);
  ]

let suite = ("Sexp", test_of_lexbuf @ test_of_string)
