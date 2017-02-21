(* This is the GADT of type equality proofs. A value of type [eq a b]
   can be viewed as a proof of the equality between the types [a] and
   [b]. It suffices to deconstruct this value -- match it against the
   pattern [Eq] -- to make the equation [a = b] available in the typing
   context. *)

type eq a b

data constructor Eq : forall a b. {} -> eq a b
  where a = b

(* Base types. *)

type int

type bool

type pair a b

program

(* Equality is symmetric and transitive. *)

let symmetry [ a ] [ b ] (p : eq a b) : eq b a =
  match p return eq b a with
  | Eq [ _ ] [ _ ] {} ->
      Eq [ b ] [ a ] {}
  end
in

let transitivity [ a ] [ b ] [ c ] (p : eq a b) (q : eq b c) : eq a c =
  match p return eq a c with
  | Eq [ _ ] [ _ ] {} ->
      match q return eq a c with
      | Eq [ _ ] [ _ ] {} ->
          Eq [ a ] [ c ] {}
      end
  end
in

(* Equality is a congruence. *)

let congruence [ a1 ] [ b1 ] [ a2 ] [ b2 ] (p1 : eq a1 b1) (p2 : eq a2 b2) : eq (pair a1 a2) (pair b1 b2) =
  match p1 return eq (pair a1 a2) (pair b1 b2) with
  | Eq [ _ ] [ _ ] {} ->
      match p2 return eq (pair a1 a2) (pair b1 b2) with
      | Eq [ _ ] [ _ ] {} ->
          Eq [ pair a1 a2 ] [ pair b1 b2 ] {}
      end
  end
in

(* If we have a proof that [int] and [bool] are equal, then anything
   is possible, and we can promise to produce a result of type [a],
   for an arbitrary [a]. This is done simply by deconstructing the
   proof. The hypothesis [int = bool] is absurd, so the [match]
   construct needs zero branches. In fact, if a branch for the data
   constructor [Eq [ _ ] [ _ ] {}] was provided, then the type-checker
   would warn that this branch is inaccessible. *)

let absurd1 [ a ] (proof : eq int bool) : a = 
  match proof return a with
  end
in

(* Absurdity can arise in indirect ways. Here, we exploit the fact that
   equality of two pair types implies equality of their components. *)

let absurd2 [ a ] (proof : eq (pair int int) (pair bool int)) : a = 
  match proof return a with
  end
in

(* Absurdity can arise in indirect ways. Here, we exploit the fact that
   equality of a type variable [a] with a type [t] implies that [a] does
   not occur in [t]. *)

let absurd3 [ a ] [ b ] (proof : eq b (pair b int)) : a = 
  match proof return a with
  end
in

(* Absurdity can arise in indirect ways. Here, we exploit the fact that
   the types [forall b.b] and [forall b.a] cannot be equal, regardless
   of the value of [a]. *)

let absurd4 [ a ] (proof : eq (forall b.b) (forall b.a)) : a = 
  match proof return a with
  end
in

absurd4

