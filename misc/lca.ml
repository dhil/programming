(* Lowest Common Ancestor *)

type tree =
  | Nil
  | Node of tree * int * tree

let leaf x = Node(Nil, x, Nil)

let ex = (* example tree *)
  Node(
      Node(leaf 1, 3, Node(leaf 4, 6, leaf 7)),
      8,
      Node(Nil, 10, Node(leaf 13, 14, Nil)))

let rec lca a' b' =
  let a = min a' b' in (* normalise input *)
  let b = max a' b' in
  function
  | Nil -> None
  | Node (_, x, _) when a <= x && b >= x ->
     Some x
  | Node (l, x, r) ->
     lca a b (if n < x && m < x then l else r)

let rec lca' m n = function
  | Nil -> None
  | Node (l, x, _) when n < x ->
     lca' m n l
  | Node (_, x, r) when m > x ->
     lca' m n r
  | Node (_, x, _) -> Some x
