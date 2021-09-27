type atom = Symbol of string | String of string | Int of int | Float of float

type t = List of t list | Atom of atom

let pp_atom fs = function
  | Symbol x -> Format.fprintf fs "%s" x
  | String x -> Format.fprintf fs "%S" x
  | Int x -> Format.fprintf fs "%i" x
  | Float x -> Format.fprintf fs "%f" x

let rec pp fs = function
  | List x ->
      Format.pp_print_list pp fs x ~pp_sep:(fun fs () -> Format.fprintf fs "@ ")
  | Atom x -> pp_atom fs x
