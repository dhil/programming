(* Given a binary tree, check whether it’s a binary search tree or not. *)

type tree =
  | Nil
  | Node of tree * int * tree

let leaf x = Node(Nil, x, Nil)

let ex = (* example tree *)
  Node(
      Node(leaf 1, 3, Node(leaf 4, 6, leaf 7)),
      8,
      Node(Nil, 10, Node(leaf 13, 14, Nil)))

let ex1 =
  Node(
      Node(leaf 1, 3, Node(leaf 4, 6, leaf 7)),
      8,
      Node(Nil, 10, Node(leaf 15, 14, Nil)))

let ex2 =
  Node(
      Node(leaf 1, 8, Node(leaf 4, 6, leaf 7)),
      8,
      Node(Nil, 10, Node(leaf 13, 14, Nil)))

let ex3 =
  Node(
    Node(leaf 1, 2, leaf 4),
    3,
    leaf 5)

let rec validate test = function
  | Nil -> true
  | Node (l, k, r) ->
     test k
     && validate (fun k' -> k' <= k && test k') l
     && validate (fun k' -> k' > k && test k') r
