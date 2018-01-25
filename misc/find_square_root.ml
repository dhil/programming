(* Find the squareroot of a given number rounded down to the nearest
   integer, without using the sqrt function. *)

let log2 x =
  (log10 x) /. (log10 2.0)

let square_root n =
  let f = float_of_int n in
  let x = 0.5 *. (log2 f) in
  int_of_float (2.0 ** x)
