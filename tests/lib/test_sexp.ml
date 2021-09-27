let ocamlformat_package =
  let open Sexp_parse.Sexp in
  List
    [
      Atom (Symbol "package");
      List [ Atom (Symbol "name"); Atom (Symbol "ocamlformat") ];
      List
        [
          Atom (Symbol "synopsis");
          Atom (String "Auto-formatter for OCaml code");
        ];
      List
        [
          Atom (Symbol "description");
          Atom
            (String
               "OCamlFormat is a tool to automatically format OCaml code in a \
                uniform style.");
        ];
      List
        [
          Atom (Symbol "depends");
          List
            [
              Atom (Symbol "ocaml");
              List
                [
                  Atom (Symbol "and");
                  List [ Atom (Symbol ">="); Atom (Float 4.08) ];
                  List [ Atom (Symbol "<"); Atom (Float 4.14) ];
                ];
            ];
          List [ Atom (Symbol "alcotest"); Atom (Symbol ":with-test") ];
          List
            [
              Atom (Symbol "base");
              List
                [
                  Atom (Symbol "and");
                  List [ Atom (Symbol ">="); Atom (Symbol "v0.12.0") ];
                  List [ Atom (Symbol "<"); Atom (Symbol "v0.15") ];
                ];
            ];
          Atom (Symbol "base-unix");
          Atom (Symbol "cmdliner");
          Atom (Symbol "dune-build-info");
          Atom (Symbol "fix");
          Atom (Symbol "fpath");
          List
            [
              Atom (Symbol "menhir");
              List [ Atom (Symbol ">="); Atom (Int 20201216) ];
            ];
          List
            [
              Atom (Symbol "menhirLib");
              List [ Atom (Symbol ">="); Atom (Int 20201216) ];
            ];
          List
            [
              Atom (Symbol "menhirSdk");
              List [ Atom (Symbol ">="); Atom (Int 20201216) ];
            ];
          List
            [
              Atom (Symbol "ocaml-version");
              List [ Atom (Symbol ">="); Atom (Symbol "3.2.0") ];
            ];
          Atom (Symbol "ocp-indent");
          List
            [
              Atom (Symbol "bisect_ppx");
              List
                [
                  Atom (Symbol "and");
                  Atom (Symbol ":with-test");
                  List [ Atom (Symbol ">="); Atom (Symbol "2.5.0") ];
                ];
            ];
          List
            [
              Atom (Symbol "odoc-parser");
              List [ Atom (Symbol ">="); Atom (Symbol "0.9.0") ];
            ];
          List
            [
              Atom (Symbol "re");
              List [ Atom (Symbol ">="); Atom (Symbol "1.7.2") ];
            ];
          List
            [
              Atom (Symbol "stdio");
              List [ Atom (Symbol "<"); Atom (Symbol "v0.15") ];
            ];
          List
            [
              Atom (Symbol "uuseg");
              List [ Atom (Symbol ">="); Atom (Symbol "10.0.0") ];
            ];
          List
            [
              Atom (Symbol "uutf");
              List [ Atom (Symbol ">="); Atom (Symbol "1.0.1") ];
            ];
        ];
    ]

let ocamlformat_rpc_lib_package =
  let open Sexp_parse.Sexp in
  List
    [
      Atom (Symbol "package");
      List [ Atom (Symbol "name"); Atom (Symbol "ocamlformat-rpc-lib") ];
      List
        [
          Atom (Symbol "synopsis");
          Atom (String "Auto-formatter for OCaml code (RPC mode)");
        ];
      List
        [
          Atom (Symbol "description");
          Atom
            (String
               "OCamlFormat is a tool to automatically format OCaml code in a \
                uniform style. This package defines a RPC interface to \
                OCamlFormat");
        ];
      List [ Atom (Symbol "license"); Atom (Symbol "MIT") ];
      List
        [
          Atom (Symbol "depends");
          List
            [
              Atom (Symbol "ocaml");
              List
                [
                  Atom (Symbol "and");
                  List [ Atom (Symbol ">="); Atom (Float 4.08) ];
                  List [ Atom (Symbol "<"); Atom (Float 4.14) ];
                ];
            ];
          Atom (Symbol "csexp");
          Atom (Symbol "sexplib0");
        ];
    ]

let test_of_lexbuf =
  let make_test ~file ~expected =
    let name = "of_lexbuf " ^ file in
    let test_fun () =
      let ic = open_in file in
      let lx = Lexing.from_channel ic in
      let result =
        Alcotest.(check (Alcotest_ext.result_msg (list Alcotest_ext.sexp)))
          name expected (Sexp_parse.of_lexbuf lx)
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
           Sexp_parse.Sexp.
             [
               List
                 [
                   Atom (Symbol "lang"); Atom (Symbol "dune"); Atom (Float 2.8);
                 ];
               List [ Atom (Symbol "name"); Atom (Symbol "ocamlformat") ];
               List
                 [
                   Atom (Symbol "using");
                   Atom (Symbol "menhir");
                   Atom (Float 2.1);
                 ];
               List [ Atom (Symbol "cram"); Atom (Symbol "enable") ];
               List
                 [ Atom (Symbol "generate_opam_files"); Atom (Symbol "true") ];
               List
                 [
                   Atom (Symbol "authors");
                   Atom (String "Josh Berdine <jjb@fb.com>");
                 ];
               List
                 [
                   Atom (Symbol "maintainers");
                   Atom
                     (String
                        "OCamlFormat Team <ocamlformat-dev@lists.ocaml.org>");
                 ];
               List
                 [
                   Atom (Symbol "source");
                   List
                     [
                       Atom (Symbol "github");
                       Atom (Symbol "ocaml-ppx/ocamlformat");
                     ];
                 ];
               ocamlformat_package;
               ocamlformat_rpc_lib_package;
             ]);
  ]

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
