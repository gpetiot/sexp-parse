[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fgpetiot%2Fsexp-parse%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/gpetiot/sexp-parse)

# sexp-parse

S-expression parsing library without the `base` dependency.


## Untyped s-expressions

Untyped s-expressions rely on the type definitions in the `sexplib0` library.

```ocaml
Untyped.of_string
  {|((data "quoted data" 123 4.5)
     (data (!@# (4.5) "(more" "data)")))|}
```

will generate:
```ocaml
Ok
  Typed.[
    List [
      List [
        Atom "data";
        Atom {|"quoted data"|};
        Atom "123";
        Atom "4.5";
      ];
      List [
        Atom "data";
        List [
          Atom "!@#";
          List [ Atom "4.5" ];
          Atom {|"(more"|};
          Atom {|"data)"|};
        ]
      ]
    ]
  ]
```


## Typed s-expressions

In typed s-expressions the atoms can be either of type `string`, `int`, `float` or `symbol` (unquoted string).

```ocaml
Typed.of_string
  {|((data "quoted data" 123 4.5)
     (data (!@# (4.5) "(more" "data)")))|}
```

will generate:
```ocaml
Ok
  Typed.[
    List [
      List [
        Atom (Symbol "data");
        Atom (String {|quoted data|});
        Atom (Int 123);
        Atom (Float 4.5);
      ];
      List [
        Atom (Symbol "data");
        List [
          Atom (Symbol "!@#");
          List [ Atom (Float 4.5) ];
          Atom (String "(more");
          Atom (String "data)");
        ]
      ]
    ]
  ]
```
