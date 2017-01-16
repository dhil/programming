(** 1 Trees, folds, and maps **)

(* (a) Define a parameterised type for representing trees *)
type 'a tree = E (* empty tree *)
               | B of 'a tree * 'a * 'a tree (* Tree with two branches and an element of type 'a *)

let my_tree = B( B( B(E, 1, E), 2, E), 3, E)
  (*
               _B_
              / | \
            _B_ 3 E
           / | \
         _B_ 2 E
        / | \
        E 1 E
  *)
                   
(* (b) Akin to [List.map] define [tree_map] for trees *)
let rec map_tree : ('a -> 'b) -> 'a tree -> 'b tree
  = fun f ->
    function
    | E -> E
    | B (lb, x, rb) -> B (map_tree f lb, f x, map_tree f rb)

let my_tree' = map_tree (fun x -> x + 1) my_tree

(* (c) Akin to [List.fold_right] define [fold_tree] for trees *)
(* Note: I believe there is a bug in the description if 
      fold_tree : ('b -> 'a -> 'b -> 'b) -> 'b -> 'a tree -> 'b
   then the expression
      fold_tree (+) 0
   is not well-typed since 
      (+) : int -> int -> int
   i.e. arity mismatch.

   I have implemented two version of [fold_tree] one with the
   aforementioned signature, and one with the more common signature:
      fold_tree : ('b -> 'a -> 'b) -> 'b -> 'a tree -> 'b
*)
let rec fold_tree' : ('b -> 'a -> 'b -> 'b) -> 'b -> 'a tree -> 'b
  = fun f u ->
    function
    | E -> u
    | B (lb, x, rb) -> f (fold_tree' f u lb) x (fold_tree' f u rb)

let rec fold_tree : ('b -> 'a -> 'b) -> 'b -> 'a tree -> 'b
  = fun f u ->
    function
    | E -> u
    | B (lb, x, rb) ->
       let rtr = fold_tree f u rb in
       let ltr = fold_tree f rtr lb in
       f ltr x
    
let tree_sums = List.map (fold_tree (+) 0) [my_tree ; my_tree']
