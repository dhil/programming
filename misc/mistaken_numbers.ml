(* Given a range 0-N find all numbers that in reverse can be mistaken
   for one another, e.g. 13 and 31. *)

let reverse_digits n =
  let rec aux acc n =
    if n > 0 then
      let d = n mod 10 in
      aux (acc * 10 + d) (n / 10)
    else
      acc
  in
  aux 0 n

let mistaken_numbers : int -> (int * int) list
  = fun n ->
    let rec loop i n acc =
      if i < n then
        let j = reverse_digits i in
          if j > i && j > 10 && j <= n then
            loop (i + 1) n ((i,j) :: acc)
          else
            loop (i + 1) n acc
      else
        acc
    in
    loop 12 n []
