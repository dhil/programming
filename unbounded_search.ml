(* Given a sorted array of unknown length and a number to search for,
   return the index of the number in the array. *)

(* Since an array is an eager data structure it must be finite,
   meaning that we can compute an upper bound for it (assumption: memory safe language). *)
(* O(log N) complexity *)
let unbounded_search : int array -> int -> int option =
  fun arr x ->
  (* Idea: Estimate an upper bound on the search by expanding the
     search region by a factor of 2. This means we only spend O(log N)
     time estimating the upper bound, where N is the size of the
     array. *)
  let rec estimate_ubound n =
    try
      if arr.(n) < x
      then estimate_ubound (n * 2)
      else n
    with Invalid_argument _ -> n (* out of bounds error *)
  in
  (* Since the array is sorted we can use a binary search to locate
     the element (if it exists) *)
  let rec binary_search : int array -> int -> int -> int -> int option
    = fun arr lbound ubound x ->
      if ubound < lbound then None
      else
        let mid = lbound + (ubound - lbound) / 2 in
        try
          if arr.(mid) = x
          then Some mid
          else if arr.(mid) > x
               then binary_search arr lbound (mid-1) x
               else binary_search arr (mid+1) ubound x
        with Invalid_argument _ ->
          binary_search arr lbound (mid-1) x
  in
  binary_search arr 0 (estimate_ubound 1) x
