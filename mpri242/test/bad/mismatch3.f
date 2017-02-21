(* Basic type mismatch. *)
program
let test [ b ] =
  let identity [ a ] (x : a) : b = x in
  identity
in
test
