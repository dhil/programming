(* Given an array of integers find the kth element in the sorted order. *)

(* Equations
  kth_largest [|3;1;2;4|] 0 = 1
  kth_largest  - = -      2 = 3
  kth_largest  - = -      3 = 4 *)

(* A solution:
    Let a be an array of integers then
    kth_largest : int array -> int -> int option
    kth_largest a k = get k . sort a
        where
          sort : int array -> int array
          get  : int -> int array -> int option

   The complexity is dominated by the sorting which is at least O(nlgn).
*)

let ascending x y =
  if x < y then -1
  else if x > y then 1
  else 0

let kth_largest : int array -> int -> int option
  = fun a k ->
    if k < Array.length a && k >= 0 then
      let a' = Array.copy a in (* lets keep the input unchanged *)
      let () = Array.sort ascending a' in
      Some a'.(k)
    else
      None

(* Median of medians approach *)
let rec select : int array -> int -> int
  = fun a k ->
    if Array.length a <= 10 then
      let () = Array.sort ascending a in
      a.(k)
    else
      let ps = partition5 a in
      let medians =
        Array.init
          (Array.length ps)
          (fun i -> select ps.(i) 2)
      in
      let median = select medians (Array.length a / 10) in
      let (l1, l2, l3) = partition median a in
      if k < Array.length l1 then
        select l1 k
      else if k > Array.length l1 + Array.length l2 then
        select l3 (k - (Array.length l1) - (Array.length l2))
      else
        median

and partition5 : int array -> int array array
  = fun a ->
    let size = Array.length a / 5 in
    Array.init
      size
      (fun i ->
        Array.sub a (i*5) 5)

and partition : int -> int array -> int array * int array * int array
  = fun pivot a ->
    let rec partition_indices i l1 l2 l3 =
      if i < Array.length a then
        if a.(i) < pivot then
          partition_indices (i+1) (i :: l1) l2 l3
        else if a.(i) > pivot then
          partition_indices (i+1) l1 l2 (i :: l3)
        else
          partition_indices (i+1) l1 (i :: l2) l3
        else
          (l1, l2, l3)
    in
    let make l =
      let len = List.length l in
      let a' =
        Array.make len 0
      in
      List.iteri
        (fun i j ->
          a'.(len - 1 - i) <- a.(j))
        l;
      a'
    in
    let (l1, l2, l3) = partition_indices 0 [] [] [] in
    (make l1, make l2, make l3)

let kth_largest' : int array -> int -> int option
  = fun a k ->
    if k < 0 || k >= Array.length a then None
    else Some (select a k)

(* Variant: Find the kth smallest element of A ∪ B.
   Source: http://typeocaml.com/2017/10/19/pearl-no-4-double-binary-search/ *)
let kth_union k a b =
  let sa = Array.length a in
  let sb = Array.length b in
  let rec kth k (a, la, ha) (b, lb, hb) =
    if la >= ha+1 then b.(k+lb)
    else if lb >= hb+1 then a.(k+la)
    else
      let ma = (ha+la)/2 and mb = (hb+lb)/2 in
      match a.(ma) < b.(mb), k >= ma-la+1+mb-lb with
      | true, true -> kth (k-(ma-la+1)) (a, ma+1, ha) (b, lb, hb)
      | true, false -> kth k (a, la, ha) (b, lb, mb-1)
      | _ -> kth k (b, lb, hb) (a, la, ha) (* swap *)
  in
  kth k (a, 0, sa-1) (b, 0, sb-1)
