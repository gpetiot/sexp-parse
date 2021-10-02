let () = Alcotest.run "sexp-parse" [ Test_typed.suite; Test_untyped.suite ]
