(* This file illustrates many of the syntactic constructs of the language.
   It is meant to exercise the parser and type-checker, but is otherwise
   not very interesting. *)

(* Data type definitions. *)

type pair a b

data constructor Pair : forall a b. { a; b } -> pair a b

type list a

data constructor Nil : forall a. {} -> list a

data constructor Cons : forall a. { a; list a } -> list a

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

(* Main program. *)

program

let identity [ a ] (x : a) = x in

let pair [ a ] [ b ] (x : a) (y : b) = Pair [ a ] [ b ] { x; y } in

let rec loop [ a ] [ b ] (x : a) : b = loop [ a ] [ b ] x in

let apply [ a ] [ b ] (f : a -> b) (x : a) : b = f x in

let id : forall a. a -> a = identity in

let di = (id) in

let di = (id : forall a. a -> a) in

let nil = fun [ a ] = Nil [ a ] {} in

let nils = Nil [ forall a. a -> a ] {} in

let di = id [ forall a. a -> a ] identity in

let k = fun [ a ] [ b ] (x : a) (y : b) = x in

let true : forall a. a -> a -> a =
  fun [ a ] (x y : a) = x
in

let false : forall a. a -> a -> a =
  fun [ a ] (x y : a) : a = (y : a)
in

let bizarre =
  fun bizarre [ a ] (x : a) (b : forall a. a -> a -> a) : a =
    b [ a ] (bizarre [ a ] x b) x
in

identity

