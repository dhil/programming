(* The unit type. *)

type unit

data constructor Unit : {} -> unit

(* Main program. *)

program

let id [ a ] (x : a) : a = x in

id [ unit ] (Unit {})

