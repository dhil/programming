(* Given a binary tree of integers, print it in level order. *)

type tree =
  | Nil
  | Node of tree * int * tree

let leaf x = Node (Nil, x, Nil)

let ex =
  Node(
    Node(leaf 4, 2, Nil),
    1,
    Node(leaf 5, 3, leaf 6))

let print_tree_level_order t =
  let q = Queue.create () in
  let cur_lvl = ref 1 in
  let next_lvl = ref 0 in
  let rec bfs () =
    if Queue.is_empty q then ()
    else
      match Queue.pop q with
      | Nil -> bfs ();
      | Node (l, k, r) ->
        Printf.printf "%d " k;
        decr cur_lvl;
        (match l, r with
        | Node _, Node _ ->
          Queue.push l q;
          Queue.push r q;
          next_lvl := !next_lvl + 2
        | (Node _ as n), _ | _, (Node _ as n) ->
          Queue.push n q;
          incr next_lvl
        | Nil, Nil -> ());
        (if !cur_lvl = 0 then
            let () = Printf.printf "\n" in
            cur_lvl := !next_lvl;
            next_lvl := 0);
        bfs ()
  in
  Queue.push t q;
  bfs ()

