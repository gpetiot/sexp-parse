type atom = Symbol of string | String of string | Int of int | Float of float

type t = List of t list | Atom of atom

val pp : Format.formatter -> t -> unit
