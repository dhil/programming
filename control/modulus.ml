(* Computing the modulus of uniform continuity using various differing
   language facilities. *)

#use "topfind";;
#require "multicont";;

(* Adapted from Bauer (https://www.youtube.com/watch?v=1j3h2vb2BRc) *)
module Ref = struct
  (* Modulus of continuity using a local reference. *)
  let mu f a =
    let r = ref 0 in
    let b n = (r := max n !r; a n) in
    ignore (f b); !r
end

module Exn = struct
  (* Modulus of continuity using exceptions & recursion. *)
  let mu f a =
    let exception Abort in
    let rec search k =
      try
        let b n = (if n <= k then a n else raise Abort) in
        ignore (f b); k
      with Abort -> search (k+1)
    in
    search 0
end

(* If OCaml had call/cc:

  (** With call/cc
      open SMLofNJ.Cont ;
      fun mu f x =
        let fun search n =
          callcc (fn k => (f (fn j => if j < n then x j else throw k (search (n+1))) ; n))
        in
          search 0
        end
   *)
*)

module Eff = struct
  (* Modulus of continuity using effect handlers *)

  type _ Effect.t += Branch : int -> unit Effect.t

  let mu f a =
    let search =
      match f (fun i -> Effect.perform (Branch i); a i) with
      | _ -> (fun st -> st)
      | effect Branch i, k ->
         (fun st -> Effect.Deep.continue k () (if i <= st then st else i))
    in
    search 0
end
