(** Adapted from "Seemingly impossible functional programs"
    (https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/) *)

#use "topfind";;
#require "multicont";;

module Bit = struct
  type t = Zero | One

  let zero : t = Zero
  let one : t = One

  let equal : t -> t -> bool
    = fun l r ->
    match (l, r) with
    | Zero, Zero | One, One -> true
    | _ -> false

  let not : t -> t = function
    | Zero -> One
    | One -> Zero

  let to_bool : t -> bool = function
    | Zero -> false
    | One -> true

  let of_bool : bool -> t = function
    | false -> Zero
    | true -> One
end

module Nat = struct
  type t = int

  let zero : t = 0
  let one : t = 1
  let equal : t -> t -> bool = Int.equal
  let max : t -> t -> t = Int.max

  let (+) : t -> t -> t = (+)
  let ( * ) : t -> t -> t = ( * )
  let (-) : t -> t -> t = (-)
end

module ExplicitLazy = struct
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

  let run_all () =
    let module Examples = Examples(Searcher(Find1)) in
    let open Examples in
    [ ex0 (); ex1 (); ex3 (); ex4 (); ex5 () ]
end

(* Adapted from Bauer (https://www.youtube.com/watch?v=1j3h2vb2BRc) *)
module Modulus = struct
  (* Modulus of continuity using a local reference. *)
  let mu f a =
    let r = ref Nat.zero in
    let b n = (r := Nat.max n !r; a n) in
    ignore (f b);
    !r
end

module Eff = struct

  module Cantor = struct
    type t = Nat.t -> Bit.t

    let cons : Bit.t -> t -> t
      = fun x xs ->
      let open Nat in
      fun i -> if Nat.equal i Nat.zero then x else xs (i - Nat.one)

    let apply : t -> Nat.t -> Bit.t
      = fun xs i -> xs i
  end

  type _ Effect.t += Search : Nat.t -> Bit.t Effect.t

  let find_neighbourhood p =
    let f =
      match p (fun n -> Effect.perform (Search n)) with
      | ans -> (fun st -> (ans, st))
      | effect Search n, k ->
         lzet open Multicont.Deep in
         let r = promote k in
         (fun (st : (Nat.t * bool) list) ->
           match List.assoc_opt n st with
           | Some v -> resume r (Bit.of_bool v) st
           | None ->
              (match resume r Bit.one ((n, true) :: st) with
               | (true, st) -> (true, st)
               | (false, _) -> resume r Bit.zero ((n, false) :: st)))
    in
    snd (f [])

  let epsilon : (Cantor.t -> bool) -> Cantor.t
    = fun p ->
    let xs = find_neighbourhood p in
    fun (n : Nat.t) ->
    match List.assoc_opt n xs with
    | Some b -> Bit.of_bool b
    | None -> Bit.one

  let forsome : (Cantor.t -> bool) -> bool =
    fun p -> p (epsilon p)
  let forall : (Cantor.t -> bool) -> bool
    = fun p -> not (forsome (fun a -> not (p a)))
  let equal : 'a. (Cantor.t -> 'a) -> (Cantor.t -> 'a) -> bool
    = fun f g -> forall (fun xs -> (f xs) = (g xs))

  module Examples = struct
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

    let ex0 () = equal f g
    let ex1 () = equal f h
    let ex2 () = equal g h
    let ex3 () = equal f f
    let ex4 () = equal g g
    let ex5 () = equal h h

    let run_all () =
      [ ex0 (); ex1 (); ex3 (); ex4 (); ex5 () ]
  end
end
