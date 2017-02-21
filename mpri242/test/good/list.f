(* Lists. *)

type list a

data constructor Nil : forall a. {} -> list a

data constructor Cons : forall a. { a; list a } -> list a

(* The unit type. *)

type unit

data constructor Unit : {} -> unit

(* Main program. *)

program

let rec loop [ a ] (u : unit) : a = loop [ a ] u in

let head [ a ] (xs : list a) : a =
  match xs return a with
  | Nil [ _ ] {} ->
      loop [ a ] (Unit {})
  | Cons [ _ ] { x; _ } ->
      x
  end
in

let tail [ a ] (xs : list a) : list a =
  match xs return list a with
  | Nil [ _ ] {} ->
      loop [ list a ] (Unit {})
  | Cons [ _ ] { _; xs } ->
      xs
  end
in

let rec map [ a ] [ b ] (f : a -> b) (xs : list a) : list b =
  match xs return list b with
  | Nil [ _ ] {} ->
      Nil [ b ] {}
  | Cons [ _ ] { x; xs } ->
      Cons [ b ] { f x; map [ a ] [ b ] f xs }
  end
in

map
