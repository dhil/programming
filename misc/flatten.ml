(* Implement the following interface in direct-style. *)

module type PROBLEM = sig
  type 'a node =
    | One of 'a
    | Many of 'a node list

  val flatten : 'a node list  -> 'a list
end

let tests (module P : PROBLEM) =
  let open P in
  assert (flatten [One 1; One 2; Many [One 3; Many [One 4; One 5; One 6]]] = [1;2;3;4;5;6]);
  assert (flatten [] = []);
  assert (flatten [One 0] = [0]);
  assert (flatten [Many []] = []);
  assert (flatten [Many []; Many []; Many [One 1; One 2]; Many [One 3; One 4]; Many []; Many []; One 5] = [1;2;3;4;5])

module Solution : PROBLEM = struct
    type 'a node =
    | One of 'a
    | Many of 'a node list

    let rec flatten : 'a node list  -> 'a list
      = fun xs ->
      match xs with
      | [] -> []
      | One x :: xs ->
         x :: (flatten xs)
      | Many ys :: xs ->
         List.append (flatten ys) (flatten xs)
end

let _ = tests (module Solution)

(* Transform `flatten` into a tail-recursive function using CPS. *)
module CPS_Solution : PROBLEM = struct
  type 'a node =
    | One of 'a
    | Many of 'a node list

  let rec flatten_cps : 'a node list -> ('a list -> 'a list) -> 'a list
    = fun xs k ->
    match xs with
    | [] -> k []
    | One x :: xs ->
       flatten_cps xs (fun xs' -> k (x :: xs'))
    | Many ys :: xs ->
       flatten_cps ys
         (fun ys' ->
           flatten_cps xs
             (fun xs' ->
               k (List.append ys' xs')))

  let flatten xs = flatten_cps xs (fun x -> x)
end

let _ = tests (module CPS_Solution)

(* Defunctionalise `flatten_cps`. *)
module Defun_Solution : PROBLEM = struct
  type 'a node =
    | One of 'a
    | Many of 'a node list

  type 'a dnode =
    | NIdentity
    | NOne of 'a dnode * 'a
    | NMany1 of 'a dnode * 'a node list
    | NMany2 of 'a dnode * 'a list

  let rec transform : 'a node list -> 'a dnode -> 'a list
    = fun xs k ->
    match xs with
    | [] ->
       apply k []
    | One x :: xs ->
       transform xs (NOne (k, x))
    | Many ys :: xs ->
       transform ys (NMany1 (k, xs))

  and apply : 'a dnode -> 'a list -> 'a list
    = fun k ys ->
    match k with
    | NIdentity ->
       ys
    | NOne (k', x) ->
       apply k' (x :: ys)
    | NMany1 (k', xs) ->
       transform xs (NMany2 (k', ys))
    | NMany2 (k', xs) ->
       apply k' (List.append xs ys)

  let flatten xs = transform xs NIdentity
end

let _ = tests (module Defun_Solution)
