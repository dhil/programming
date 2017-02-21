(* Basic type mismatch. *)

type int

type bool

program
let f (x : int) : bool = x in
f
