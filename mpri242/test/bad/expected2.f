(* Expected a universal type, got an arrow. *)
program
let identity [ a ] (x : a) : a = x in
fun [ a ] = identity [ a ] [ a ]
