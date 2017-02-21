(* Expected an arrow type, got a universal. *)
program
let identity [ a ] (x : a) : a = x in
identity identity
