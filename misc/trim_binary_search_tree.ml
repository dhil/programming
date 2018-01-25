(* Given the root of a binary search tree and 2 numbers min and max,
   trim the tree such that all the numbers in the new tree are between
   min and max (inclusive). *)

type tree =
  | Nil
  | Node of tree * int * tree

let leaf x = Node(Nil, x, Nil)

let ex = (* example tree *)
  Node(
      Node(leaf 1, 3, Node(leaf 4, 6, leaf 7)),
      8,
      Node(Nil, 10, Node(leaf 13, 14, Nil)))

let rec trim_tree min_val max_val = function
  | Nil -> Nil
  | Node (l, k, r) ->
     let l = trim_tree min_val max_val l in
     let r = trim_tree min_val max_val r in
     if min_val <= k && k <= max_val then
       Node (l, k, r)
     else if k < min_val then r
     else l
