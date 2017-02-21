(* This is a classic GADT where the type of a list is indexed with its
   length, encoded as a unary number. *)

(* The type constructors [zero] and [succ] are used to encode type-level
   natural numbers. *)

type zero

type succ n

(* Indexed lists. *)

type list n a

data constructor Nil : forall a. {} -> list zero a

data constructor Cons : forall n a. { a; list n a } -> list (succ n) a

(* Main program. *)

program

let head [ n ] [ a ] (xs : list (succ n) a) : a =
  match xs return a with
  | Cons [ _ ] [ _ ] { x; _ } ->
      x
  end
in

let tail [ n ] [ a ] (xs : list (succ n) a) : list n a =
  match xs return list n a with
  | Cons [ _ ] [ _ ] { _; xs } ->
      xs
  end
in

let rec map [ n ] [ a ] [ b ] (f : a -> b) (xs : list n a) : list n b =
  match xs return list n b with
  | Nil [ _ ] {} ->
      Nil [ b ] {}
  | Cons [ n ] [ _ ] { x; xs } ->
      Cons [ n ] [ b ] { f x; map [ n ] [ a ] [ b ] f xs }
  end
in

map
