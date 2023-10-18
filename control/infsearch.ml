(** Adapted from "Seemingly impossible functional programs"
    (https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/) *)

module Bit = struct
  type t = Zero | One
  let equal : t -> t -> bool
    = fun l r ->
    match (l, r) with
    | Zero, Zero | One, One -> true
    | _ -> false
end

module Nat = struct
  type t = int

  let zero : t = 0
  let one : t = 1

  let (+) : t -> t -> t = (+)
  let ( * ) : t -> t -> t = ( * )
  let (-) : t -> t -> t = (-)
end

module Cantor = struct
  type t = (Nat.t -> Bit.t) Lazy.t

  let cons : Bit.t -> t -> t
    = fun x xs ->
    let open Nat in
    lazy (fun i -> if Int.equal i 0 then x else (Lazy.force xs) (i - 1))

  let apply : t -> Nat.t -> Bit.t
    = fun xs i ->
    (Lazy.force xs) i
end

module type FINDER = sig
  val find : (Cantor.t -> bool) -> Cantor.t
end

module type SEARCHER = sig
  val forsome : (Cantor.t -> bool) -> bool
  val forall : (Cantor.t -> bool) -> bool
  val search : (Cantor.t -> bool) -> Cantor.t option
  val equal : (Cantor.t -> 'a) -> (Cantor.t -> 'a) -> bool
end

module Searcher(F : FINDER) = struct

  let forsome : (Cantor.t -> bool) -> bool
    = fun p -> p (F.find p)

  let forall p = not (forsome (fun xs -> not (p xs)))

  let search : (Cantor.t -> bool) -> Cantor.t option
    = fun p ->
    if forsome p
    then Some (F.find p)
    else None

  let equal : 'a. (Cantor.t -> 'a) -> (Cantor.t -> 'a) -> bool
    = fun f g -> forall (fun xs -> (f xs) = (g xs))
end

module rec Find1 : FINDER = struct
  module S : SEARCHER = Searcher(Find1)
  let rec find p = lazy (Lazy.force (if S.forsome (fun xs -> p (Cantor.cons Bit.Zero xs))
                                     then Cantor.cons Bit.Zero (find (fun xs -> p (Cantor.cons Bit.Zero xs)))
                                     else Cantor.cons Bit.One (find (fun xs -> p (Cantor.cons Bit.One xs)))))
end

module Examples(S : SEARCHER) = struct
  let coerce : Bit.t -> Nat.t = function
    | Bit.Zero -> Nat.zero
    | Bit.One -> Nat.one

  let f (xs : Cantor.t) : Nat.t =
    let open Nat in
    coerce (Cantor.apply xs (7 * coerce (Cantor.apply xs 4) + 4 * (coerce (Cantor.apply xs 7)) + 4))
  let g (xs : Cantor.t) : Nat.t =
    let open Nat in
    coerce (Cantor.apply xs (coerce (Cantor.apply xs 4) + 11 * (coerce (Cantor.apply xs 7))))
  let h (xs : Cantor.t) : Nat.t  =
    if Bit.equal (Cantor.apply xs 7) Bit.Zero
    then if Bit.equal (Cantor.apply xs 4) Bit.Zero then coerce (Cantor.apply xs 4) else coerce (Cantor.apply xs 11)
    else if Bit.equal (Cantor.apply xs 4) Bit.One then coerce (Cantor.apply xs 15) else coerce (Cantor.apply xs 8)

  let ex0 () = S.equal f g
  let ex1 () = S.equal f h
  let ex2 () = S.equal g h
  let ex3 () = S.equal f f
  let ex4 () = S.equal g g
  let ex5 () = S.equal h h
end
