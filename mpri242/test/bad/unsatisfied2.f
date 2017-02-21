(* Unsatisfied equation -- that is, attempt to apply a data constructor
   without satisfying the equations that it requires. *)

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

type int

type bool

program
let forge [ a ] [ b ] [ c ] (p : eq a b) : eq a c =
  match p return eq a c with
  | Eq [ _ ] [ _ ] {} ->
      Eq [ a ] [ c ] {}
  end
in
forge [ int ] [ int ] [ bool ] (Eq [ int ] [ int ] {})

