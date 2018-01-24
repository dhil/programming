(* Given an array of integers (positive and negative) find the largest
   continuous sum. *)

let sum a =
  let rec loop a i sum sums =
    if i < Array.length a then
      if a.(i) < 0 then
        loop a (i+1) 0 (sum :: sums)
      else
        loop a (i+1) (a.(i) + sum) sums
    else
      sum :: sums
  in
  List.fold_left
    (fun max n ->
      if n > max then n
      else max)
    0 (loop a 0 0 [])
