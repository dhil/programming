(* Wrong number of type arguments for data constructor. *)

type pair a b

data constructor Pair : forall a b. { a; b } -> pair a b

type list a

data constructor Nil : forall a. {} -> list a

data constructor Cons : forall a. { a; list a } -> list a

type int

program

Nil {}

