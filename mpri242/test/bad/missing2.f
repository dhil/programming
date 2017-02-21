(* Missing cases in pattern matching. *)

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

program

fun absurd [ a ] [ b ] [ c ] (p : eq a b) : c =
  match p return c with
  end

