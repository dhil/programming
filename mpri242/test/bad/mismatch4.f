(* Basic type mismatch, in the presence of some equations. *)

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

program
let test [ a ] [ b ] (p : eq a b ) =
  match p return a with
  | Eq [ _ ] [ _ ] {} ->
      fun (x : a) = x
  end
in
test
