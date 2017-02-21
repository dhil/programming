(* Missing cases in pattern matching. *)

type list a

data constructor Nil : forall a. {} -> list a

data constructor Cons : forall a. { a; list a } -> list a

program

fun head [ a ] (xs : list a) : a =
  match xs return a with
  | Cons [ _ ] { x; _ } ->
      x
  end

