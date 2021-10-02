let ocamlformat_package =
  let open Sexplib0.Sexp in
  List
    [
      Atom "package";
      List [ Atom "name"; Atom "ocamlformat" ];
      List [ Atom "synopsis"; Atom "\"Auto-formatter for OCaml code\"" ];
      List
        [
          Atom "description";
          Atom
            "\"OCamlFormat is a tool to automatically format OCaml code in a \
             uniform style.\"";
        ];
      List
        [
          Atom "depends";
          List
            [
              Atom "ocaml";
              List
                [
                  Atom "and";
                  List [ Atom ">="; Atom "4.08" ];
                  List [ Atom "<"; Atom "4.14" ];
                ];
            ];
          List [ Atom "alcotest"; Atom ":with-test" ];
          List
            [
              Atom "base";
              List
                [
                  Atom "and";
                  List [ Atom ">="; Atom "v0.12.0" ];
                  List [ Atom "<"; Atom "v0.15" ];
                ];
            ];
          Atom "base-unix";
          Atom "cmdliner";
          Atom "dune-build-info";
          Atom "fix";
          Atom "fpath";
          List [ Atom "menhir"; List [ Atom ">="; Atom "20201216" ] ];
          List [ Atom "menhirLib"; List [ Atom ">="; Atom "20201216" ] ];
          List [ Atom "menhirSdk"; List [ Atom ">="; Atom "20201216" ] ];
          List [ Atom "ocaml-version"; List [ Atom ">="; Atom "3.2.0" ] ];
          Atom "ocp-indent";
          List
            [
              Atom "bisect_ppx";
              List
                [
                  Atom "and";
                  Atom ":with-test";
                  List [ Atom ">="; Atom "2.5.0" ];
                ];
            ];
          List [ Atom "odoc-parser"; List [ Atom ">="; Atom "0.9.0" ] ];
          List [ Atom "re"; List [ Atom ">="; Atom "1.7.2" ] ];
          List [ Atom "stdio"; List [ Atom "<"; Atom "v0.15" ] ];
          List [ Atom "uuseg"; List [ Atom ">="; Atom "10.0.0" ] ];
          List [ Atom "uutf"; List [ Atom ">="; Atom "1.0.1" ] ];
        ];
    ]

let ocamlformat_rpc_lib_package =
  let open Sexplib0.Sexp in
  List
    [
      Atom "package";
      List [ Atom "name"; Atom "ocamlformat-rpc-lib" ];
      List
        [ Atom "synopsis"; Atom "\"Auto-formatter for OCaml code (RPC mode)\"" ];
      List
        [
          Atom "description";
          Atom
            "\"OCamlFormat is a tool to automatically format OCaml code in a \
             uniform style. This package defines a RPC interface to \
             OCamlFormat\"";
        ];
      List [ Atom "license"; Atom "MIT" ];
      List
        [
          Atom "depends";
          List
            [
              Atom "ocaml";
              List
                [
                  Atom "and";
                  List [ Atom ">="; Atom "4.08" ];
                  List [ Atom "<"; Atom "4.14" ];
                ];
            ];
          Atom "csexp";
          Atom "sexplib0";
        ];
    ]

let test_of_lexbuf =
  let make_test ~file ~expected =
    let name = "of_lexbuf " ^ file in
    let test_fun () =
      let ic = open_in file in
      let lx = Lexing.from_channel ic in
      let result =
        Alcotest.(check (Alcotest_ext.result_msg (list Alcotest_ext.sexp0)))
          name expected
          (Sexp_parse.Untyped.of_lexbuf lx)
      in
      close_in ic;
      result
    in
    (name, `Quick, test_fun)
  in
  [
    make_test ~file:"ocamlformat-dune-project"
      ~expected:
        (Ok
           Sexplib0.Sexp.
             [
               List [ Atom "lang"; Atom "dune"; Atom "2.8" ];
               List [ Atom "name"; Atom "ocamlformat" ];
               List [ Atom "using"; Atom "menhir"; Atom "2.1" ];
               List [ Atom "cram"; Atom "enable" ];
               List [ Atom "generate_opam_files"; Atom "true" ];
               List [ Atom "authors"; Atom "\"Josh Berdine <jjb@fb.com>\"" ];
               List
                 [
                   Atom "maintainers";
                   Atom "\"OCamlFormat Team <ocamlformat-dev@lists.ocaml.org>\"";
                 ];
               List
                 [
                   Atom "source";
                   List [ Atom "github"; Atom "ocaml-ppx/ocamlformat" ];
                 ];
               ocamlformat_package;
               ocamlformat_rpc_lib_package;
             ]);
  ]

let test_of_string =
  let make_test ~name ~input ~expected =
    let name = "of_string " ^ name in
    let test_fun () =
      Alcotest.check
        (Alcotest_ext.result_msg (Alcotest.list Alcotest_ext.sexp0))
        name expected
        (Sexp_parse.Untyped.of_string input)
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
           [
             Sexplib0.Sexp.List
               [
                 List
                   [
                     Atom "data"; Atom {|"quoted data"|}; Atom "123"; Atom "4.5";
                   ];
                 List
                   [
                     Atom "data";
                     List
                       [
                         Atom "!@#";
                         List [ Atom "4.5" ];
                         Atom {|"(more"|};
                         Atom {|"data)"|};
                       ];
                   ];
               ];
           ]);
  ]

let suite = ("Untyped", test_of_lexbuf @ test_of_string)
