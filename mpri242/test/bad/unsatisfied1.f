(* Unsatisfied equation -- that is, attempt to apply a data constructor
   without satisfying the equations that it requires. *)

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

type int

type bool

program
let forge [ a ] [ b ] : eq a b =
  Eq [ a ] [ b ] {}
in
forge [ int ] [ bool ]

