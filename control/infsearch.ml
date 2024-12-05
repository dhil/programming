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
  let less_than : t -> t -> bool
    = fun x y ->
    (Int.compare x y) < 0
  let max : t -> t -> t = Int.max

  let (+) : t -> t -> t = (+)
  let ( * ) : t -> t -> t = ( * )
  exception Underflow
  let (-) : t -> t -> t
    = fun x y ->
    let ans = x - y in
    if ans < 0 then raise Underflow
    else ans
end

(* Adapted from Bauer (https://www.youtube.com/watch?v=1j3h2vb2BRc) *)
(* module Pruned = struct *)
(*   (\* Modulus of continuity using a local reference. *\) *)
(*   let mu f a = *)
(*     let r = ref Nat.zero in *)
(*     let b n = (r := Nat.max n !r; a n) in *)
(*     ignore (f b); !r *)

(*   (\* Modulus of continuity using exceptions & recursion. *\) *)
(*   let mu' f a = *)
(*     let exception Abort in *)
(*     let rec search k = *)
(*       try *)
(*         let b n = (if n < k then a n else raise Abort) in *)
(*         ignore (f b); k *)
(*       with Abort -> search (k+1) *)
(*     in *)
(*     search 0 *)

(*   (\** With call/cc *)
(*       open SMLofNJ.Cont ; *)
(*       fun mu f x = *)
(*         let fun search n = *)
(*           callcc (fn k => (f (fn j => if j < n then x j else throw k (search (n+1))) ; n)) *)
(*         in *)
(*           search 0 *)
(*         end *)
(*    *\) *)
(* end *)

module Cantor = struct
  type t = Nat.t -> Bit.t

  let cons : Bit.t -> t -> t
    = fun x xs ->
    let open Nat in
    fun i -> if Nat.equal i Nat.zero then x else xs (i - Nat.one)

  let apply : t -> Nat.t -> Bit.t
    = fun xs i -> xs i
end


module type FINDER = sig
  (* Selection function *)
  val epsilon : (Cantor.t -> bool) -> Cantor.t
end

module type SEARCHER = sig
  val forsome : (Cantor.t -> bool) -> bool
  val forall : (Cantor.t -> bool) -> bool
  val equal : (Cantor.t -> 'a) -> (Cantor.t -> 'a) -> bool
  val modulus : (Cantor.t -> int) -> Nat.t
end

module Make(S : FINDER) = struct
  let forsome : (Cantor.t -> bool) -> bool =
    fun p -> p (S.epsilon p)

  let forall : (Cantor.t -> bool) -> bool
    = fun p -> not (forsome (fun a -> not (p a)))

  let equal : 'a. (Cantor.t -> 'a) -> (Cantor.t -> 'a) -> bool
    = fun f g -> forall (fun xs -> (f xs) = (g xs))

  let modulus : (Cantor.t -> int) -> Nat.t
    = fun f ->
    let rec least : (Nat.t -> bool) -> Nat.t
      = fun p ->
      if p Nat.zero then Nat.zero
      else let open Nat in
           Nat.one + (least (fun n -> p (n + Nat.one)))
    in
    let implies : bool -> bool -> bool
      = fun x y ->
      (not x) || y
    in
    let rec eq : Nat.t -> Cantor.t -> Cantor.t -> bool
      = fun n xs ys ->
      let open Nat in
      equal n Nat.zero || (let n = n - 1 in (Bit.equal (xs n) (ys n)) && eq n xs ys)
    in
    least
      (fun n ->
        forall
          (fun xs ->
            forall
              (fun ys ->
                implies (eq n xs ys) (f xs = f ys))))
end

let coerce : Bit.t -> Nat.t = function
  | Bit.Zero -> Nat.zero
  | Bit.One -> Nat.one

let proj : Nat.t -> (Cantor.t -> int)
  = fun i xs -> coerce (xs i)

module Modulus = struct
  let modulus : (Cantor.t -> bool) -> (Nat.t * bool) list -> (bool * (Nat.t * bool) list)
    = fun p st0 ->
    let st = ref st0 in
    let q n =
      match List.assoc_opt n !st with
      | Some v -> Bit.of_bool v
      | None ->
         st := (n, true) :: !st; Bit.one
    in
    let ans = p q in
    (ans, !st)

  let rec next_point = function
    | [] -> []
    | (n, true) :: st -> (n, false) :: st
    | (n, false) :: st -> next_point st

  let find_neighbourhood p =
    let rec loop p st =
      let (ans, st') = modulus p st in
      if ans then (ans, st')
      else match next_point st' with
           | [] -> (ans, st')
           | st'' -> loop p st''
    in
    loop p []

  let epsilon : (Cantor.t -> bool) -> Cantor.t
    = fun p ->
    let xs = find_neighbourhood p in
    fun (n : Nat.t) ->
    match List.assoc_opt n (snd xs) with
    | Some b -> Bit.of_bool b
    | None -> Bit.one
end

module Effectful = struct
  type _ Effect.t += Search : Nat.t -> Bit.t Effect.t

  let find_neighbourhood p =
    let f =
      match p (fun n -> Effect.perform (Search n)) with
      | ans -> (fun st -> (ans, st))
      | effect Search n, k ->
         let open Multicont.Deep in
         let r = promote k in
         (fun (st : (Nat.t * bool) list) ->
           match List.assoc_opt n st with
           | Some v -> resume r (Bit.of_bool v) st
           | None ->
              (match resume r Bit.one ((n, true) :: st) with
               | (true, st) -> (true, st)
               | (false, _) -> resume r Bit.zero ((n, false) :: st)))
    in
    f []

  (* Potentially faster version using reference cells *)
  let find_neighbourhood' p =
    let st = Sys.opaque_identity (ref []) in
    let ans = match p (fun n -> Effect.perform (Search n)) with
      | ans -> (ans, !st)
      | effect Search n, k ->
         let open Multicont.Deep in
         let r = promote k in
         match List.assoc_opt n !st with
         | Some v ->
            resume r (Bit.of_bool v)
         | None ->
            let old_st = !st in
            st := (n, true) :: !st;
            match resume r Bit.one with
            | (true, st) -> (true, st)
            | (false, _) ->
               st := (n, false) :: old_st;
               resume r Bit.zero
    in
    ans

  let epsilon : (Cantor.t -> bool) -> Cantor.t
    = fun p ->
    let xs = find_neighbourhood p in
    fun (n : Nat.t) ->
    match List.assoc_opt n (snd xs) with
    | Some b -> Bit.of_bool b
    | None -> Bit.one
end

module LazyCantor = struct
  type t = (Nat.t Lazy.t -> Bit.t Lazy.t) Lazy.t

  let apply : t -> Nat.t Lazy.t -> Bit.t Lazy.t
    = fun xs i ->
    (Lazy.force xs) i

  let cons : Bit.t Lazy.t -> t -> t
    = fun x xs ->
    let open Nat in
    lazy (fun i ->
        let i' = Lazy.force i in
        if Int.equal i' 0 then x else apply xs (lazy (i' - 1)))
end

module LazyFun1 = struct
  let rec epsilon : (LazyCantor.t -> bool) -> LazyCantor.t
    = fun p ->
    lazy (Lazy.force (if let p' xs = p (LazyCantor.cons (lazy Bit.zero) xs) in
                         p' (epsilon p')
                      then LazyCantor.cons (lazy Bit.zero) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.zero) xs)))
                      else LazyCantor.cons (lazy Bit.one) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.one) xs)))))
end

module LazyFun2 = struct
  let rec epsilon : (LazyCantor.t -> bool) -> LazyCantor.t
    = fun p ->
    lazy (Lazy.force (if p (LazyCantor.cons (lazy Bit.zero) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.zero) xs))))
                      then LazyCantor.cons (lazy Bit.zero) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.zero) xs)))
                      else LazyCantor.cons (lazy Bit.one) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.one) xs)))))
end

module LazyFun3 = struct
  let rec epsilon : (LazyCantor.t -> bool) -> LazyCantor.t
    = fun p ->
    let h : Bit.t Lazy.t =
      lazy (if p (LazyCantor.cons (lazy Bit.zero) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.zero) xs))))
            then Bit.zero else Bit.one)
    in
    lazy (fun i ->
        let xs = LazyCantor.cons h (epsilon (fun xs -> p (LazyCantor.cons h xs))) in
        LazyCantor.apply xs i)
end

module LazyFun4 = struct
  let rec epsilon : (LazyCantor.t -> bool) -> LazyCantor.t
    = fun p ->
    lazy
      (let left_branch =
         LazyCantor.cons (lazy Bit.zero) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.zero) xs)))
       in
       (Lazy.force
          (if p left_branch
           then left_branch
           else LazyCantor.cons (lazy Bit.one) (epsilon (fun xs -> p (LazyCantor.cons (lazy Bit.one) xs))))))
end

module LazyFun5 = struct
  let rec epsilon : (LazyCantor.t -> bool) -> LazyCantor.t
    = fun p ->
    let q : Nat.t Lazy.t -> LazyCantor.t -> bool
      = fun n xs ->
      let ys =
        lazy (fun i ->
            let i' = Lazy.force i in
            let n' = Lazy.force n in
            if Nat.less_than i' n'
            then let f = Lazy.force (epsilon p) in
                 f i
            else if Nat.equal i' n'
            then lazy Bit.zero
            else lazy Bit.one)
      in
      p ys
    in
    lazy (fun n ->
        lazy (let ans = q n (epsilon (q n)) in
              if ans then Bit.zero else Bit.one))
end

module LazyFun6 = struct
  let rec epsilon : (LazyCantor.t -> bool Lazy.t) -> LazyCantor.t
    = fun p ->
    let rec b : LazyCantor.t
      = lazy
          (fun n ->
            lazy
              (if Lazy.force (q n (epsilon (q n))) then Bit.zero else Bit.one))
    and q : Nat.t Lazy.t -> LazyCantor.t -> bool Lazy.t
      = fun n xs ->
      let ys =
        lazy (fun i ->
            let i' = Lazy.force i in
            let n' = Lazy.force n in
            if Nat.less_than i' n'
            then let f = Lazy.force b in
                 f i
            else if Nat.equal i' n' then lazy Bit.zero
            else LazyCantor.apply xs (lazy Nat.(i' - n' - 1)))
      in
      p (lazy (fun i -> LazyCantor.apply ys i))
    in
    b
end

module Lift_lazy(M : sig
             val epsilon : (LazyCantor.t -> bool) -> LazyCantor.t
           end) = struct
  let epsilon : (Cantor.t -> bool) -> Cantor.t
    = fun p ->
    let xs =
      M.epsilon (fun xs -> p (fun i -> Lazy.force (LazyCantor.apply xs (lazy i))))
    in
    (fun i -> Lazy.force (LazyCantor.apply xs (lazy i)))
end

module Lift_lazy2(M : sig
             val epsilon : (LazyCantor.t -> bool Lazy.t) -> LazyCantor.t
           end) = struct
  let epsilon : (Cantor.t -> bool) -> Cantor.t
    = fun p ->
    let ans =
      M.epsilon (fun xs -> lazy (p (fun i -> Lazy.force (LazyCantor.apply xs (lazy i)))))
    in
    (fun i -> Lazy.force (LazyCantor.apply ans (lazy i)))
end

module Modsearch = Make(Modulus)
module Effsearch = Make(Effectful)
module Lazysearch1 = Make(Lift_lazy(LazyFun1))
module Lazysearch2 = Make(Lift_lazy(LazyFun2))
module Lazysearch3 = Make(Lift_lazy(LazyFun3))
module Lazysearch4 = Make(Lift_lazy(LazyFun4))
module Lazysearch5 = Make(Lift_lazy(LazyFun5))
module Lazysearch6 = Make(Lift_lazy2(LazyFun6))

module Test = struct
  let searchers : (string * (module SEARCHER)) list =
    [ "modsearch", (module Modsearch)
    ; "effsearch", (module Effsearch)
    ; "lazysearch_1", (module Lazysearch1)
    ; "lazysearch_2", (module Lazysearch2)
    ; "lazysearch_3", (module Lazysearch3)
    ; "lazysearch_4", (module Lazysearch4)
    ; "lazysearch_5", (module Lazysearch5) ]

  let run_tests' equal =
    let f (xs : Cantor.t) : Nat.t =
      let open Nat in
      coerce (Cantor.apply xs (7 * coerce (Cantor.apply xs 4) + 4 * (coerce (Cantor.apply xs 7)) + 4))
    in
    let g (xs : Cantor.t) : Nat.t =
      let open Nat in
      coerce (Cantor.apply xs (coerce (Cantor.apply xs 4) + 11 * (coerce (Cantor.apply xs 7))))
    in
    let h (xs : Cantor.t) : Nat.t  =
      if Bit.equal (Cantor.apply xs 7) Bit.Zero
      then if Bit.equal (Cantor.apply xs 4) Bit.Zero then coerce (Cantor.apply xs 4) else coerce (Cantor.apply xs 11)
      else if Bit.equal (Cantor.apply xs 4) Bit.One then coerce (Cantor.apply xs 15) else coerce (Cantor.apply xs 8)
    in
    let ex0 () = equal f g in
    let ex1 () = equal f h in
    let ex2 () = equal g h in
    let ex3 () = equal f f in
    let ex4 () = equal g g in
    let ex5 () = equal h h in
    [ ex0 (); ex1 (); ex2 (); ex3 (); ex4 (); ex5 () ]

  let run_tests () =
    let results =
      List.map (fun (desc, (module S : SEARCHER)) -> (desc, run_tests' S.equal)) searchers
    in
    let rec pairings xs =
      match xs with
      | [] -> []
      | x :: xs ->
         let rec pair_with x = function
           | [] -> []
           | y :: ys -> (x, y) :: pair_with x ys
         in
         pair_with x xs :: pairings xs
    in
    let rec check xs =
      match xs with
      | [] -> ()
      | ((desc, results), (desc', results')) :: xs ->
         if List.equal (=) results results' then check xs
         else Printf.printf "test error: %s disagrees with %s\n%!" desc desc'
    in
    check (List.concat (pairings results))
end
