type atom = Symbol of string | String of string | Int of int | Float of float

type t = List of t list | Atom of atom

let eq_atom x y =
  match (x, y) with
  | Symbol x, Symbol y -> String.equal x y
  | String x, String y -> String.equal x y
  | Int x, Int y -> Int.equal x y
  | Float x, Float y -> Float.equal x y
  | _ -> false

let rec eq_sexp x y =
  match (x, y) with
  | Atom x, Atom y -> eq_atom x y
  | List x, List y -> eq_sexp_list x y
  | _ -> false

and eq_sexp_list x y =
  match (x, y) with
  | [], [] -> true
  | a :: b, c :: d -> eq_sexp a c && eq_sexp_list b d
  | _ -> false

let equal = eq_sexp

let pp_atom fs = function
  | Symbol x -> Format.fprintf fs "%s" x
  | String x -> Format.fprintf fs "%S" x
  | Int x -> Format.fprintf fs "%i" x
  | Float x -> Format.fprintf fs "%f" x

let pp_sep fs () = Format.fprintf fs "@ "

let rec pp fs = function
  | List x ->
      Format.fprintf fs "@[<hv 2>(%a)@]" (Format.pp_print_list pp ~pp_sep) x
  | Atom x -> pp_atom fs x

let dump_atom fs = function
  | Symbol x -> Format.fprintf fs "Symbol %s" x
  | String x -> Format.fprintf fs "String %S" x
  | Int x -> Format.fprintf fs "Int %i" x
  | Float x -> Format.fprintf fs "Float %f" x

let rec dump fs = function
  | List x ->
      Format.fprintf fs "@[<hv 2>(%a)@]" (Format.pp_print_list dump ~pp_sep) x
  | Atom x -> dump_atom fs x

let atom_to_string = function
  | Symbol x -> x
  | String x -> Format.sprintf "%S" x
  | Int x -> Int.to_string x
  | Float x -> Float.to_string x

let rec to_sexplib0 = function
  | List x -> Sexplib0.Sexp.List (List.map to_sexplib0 x)
  | Atom x -> Sexplib0.Sexp.Atom (atom_to_string x)
