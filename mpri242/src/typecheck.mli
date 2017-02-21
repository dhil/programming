open Terms
open Types

(* The type-checker checks that a complete program is well-typed. *)

(* Furthermore, the type-checker records information at function and
   application nodes, as explained in [Terms]. This is done by
   writing the references that exist at these nodes. *)

(* The type-checker returns the inferred type of the program, together
   with an export environment that allows printing this type if
   desired. *)

val run: program -> Export.env * ftype

