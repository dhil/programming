(* Given a stream of unsorted integers, find the median element in
   sorted order at any given time. *)

(* Idea use two heaps. *)

module Heap = struct
  type 'a t =
    | Nil
    | Node of 'a t * 'a * 'a t * int

  let empty = Nil

  let singleton x =
    Node (Nil, x, Nil, 1)

  let rank = function
    | Nil -> 0
    | Node (_,_,_,r) -> r

  let rec merge h1 h2 =
    match h1, h2 with
    | Nil, t | t, Nil -> t
    | Node (l, x, r, _), Node (_, y, _, _) ->
       if x > y then merge h2 h1 (* normalise *)
       else
         let merged = merge r h2 in
         let rank_left = rank l in
         let rank_right = rank merged in
         if rank_left >= rank_right then
           Node (l, x, merged, rank_right+1)
         else
           Node (merged, x, l, rank_left+1)

  let insert x h =
    merge (singleton x) h

  let get_min = function
    | Nil -> None
    | Node (_, x, _, _) -> Some x

  let delete_min = function
    | Nil -> empty
    | Node (l, _, r, _) ->
       merge l r

  let is_empty = function
    | Nil -> true
    | _   -> false
end

(* We model the stream using a list *)
let median_of_stream : int list -> float
  = fun xs ->
    (* The following function simulates streaming *)
    let rec exhaust n min_heap max_heap = function
      | [] -> (min_heap, max_heap), n
      | x :: xs when n mod 2 = 0 ->
         let max_heap =
           (* The -1 factor makes the min leftist heap behaves like a
              max heap. *)
           Heap.insert ((-1) * x) max_heap
         in
         let n = n + 1 in
         if not (Heap.is_empty min_heap) then
           let to_min, to_max =
             match Heap.get_min max_heap, Heap.get_min min_heap with
             | Some x, Some y -> (-1) * x, y
             | _, _ -> assert false
           in
           if to_min > to_max then
             let max_heap = Heap.delete_min max_heap in
             let min_heap = Heap.delete_min min_heap in
             exhaust n
               (Heap.insert to_min min_heap)
               (Heap.insert ((-1) * to_max) max_heap)
               xs
           else
             exhaust n min_heap max_heap xs
         else
           exhaust n min_heap max_heap xs
      | x ::  xs ->
         let max_heap =
           Heap.insert ((-1) * x) max_heap
         in
         let to_min =
           match Heap.get_min max_heap with
           | None -> assert false
           | Some x -> (-1) * x
         in
         let max_heap =
           Heap.delete_min max_heap
         in
         let min_heap =
           Heap.insert to_min min_heap
         in
         exhaust (n+1) min_heap max_heap xs
    in
    let (min_heap, max_heap), n =
      exhaust 0 Heap.empty Heap.empty xs
    in
    (* The stream is exhausted; now we compute the median. *)
    if n mod 2 = 0 then
      let from_max, from_min =
        match Heap.get_min max_heap, Heap.get_min min_heap with
        | Some x, Some y ->
           float_of_int ((-1) * x), float_of_int y
        | _, _ -> assert false
      in
      (from_max +. from_min) /. 2.0
    else
      match Heap.get_min max_heap with
      | None -> assert false
      | Some x -> float_of_int ((-1) * x)

