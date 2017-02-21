(* Basic type mismatch. *)
program
let identity [ a ] (x : a) : a -> a = x in
identity
