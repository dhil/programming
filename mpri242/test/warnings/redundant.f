(* Redundant cases in pattern matching. *)

type list a

data constructor Nil : forall a. {} -> list a

data constructor Cons : forall a. { a; list a } -> list a

program

fun map [ a ] [ b ] (f : a -> b) (xs : list a) : list b =
  match xs return list b with
  | Nil [ _ ] {} ->
      Nil [ b ] {}
  | Cons [ _ ] { x; xs } ->
      Cons [ b ] { f x; map [ a ] [ b ] f xs }
  | Nil [ _ ] {} ->
      map [ a ] [ b ] f xs
  end
