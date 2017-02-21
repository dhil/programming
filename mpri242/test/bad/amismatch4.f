(* Wrong number of term arguments for data constructor. *)

type pair a b

data constructor Pair : forall a b. { a; b } -> pair a b

type list a

data constructor Nil : forall a. {} -> list a

data constructor Cons : forall a. { a; list a } -> list a

type int

program

fun [ a ] (x : a) =
  match Cons [ a ] { x; Nil [ a ] {} } return a with
  | Cons [ _ ] { x; y; z } ->
      x
  end

