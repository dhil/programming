(* Given a matrix of integers and coordinates of a rectangular region
   within the matrix, find the sum of numbers falling inside the
   rectangle. *)

module Rectangle = struct
  type t = int array array

  let make ?(seed = 0) m n =
    Array.make_matrix m n seed

  let init f m n =
    Array.mapi
      (fun i row ->
        Array.mapi (fun j _ -> f i j) row)
      (make m n)

  let region_sum_naive rect (i,j) (i',j') =
    let acc = ref 0 in
    for a = i to i' do
      for b = j to j' do
        acc := rect.(a).(b) + !acc
      done
    done;
    !acc

  (* Idea: successively narrow the rectangle until we have a trivial 1
     x 1 rectangle. *)
  let rec region_sum rect (i,j) (i',j') =
    if i > i' || j > j' then 0
    else if i = i' && j = j' then
      rect.(i).(j)
    else
      let top_left = (* O(mn / 4) *)
        let i'', j'' = i' / 2, j' / 2 in
        region_sum rect (i, j) (i'', j'')
      in
      let top_right = (* O(mn / 4) *)
        let i'', j'' = i' / 2, j' / 2 + 1 in
        region_sum rect (i, j'') (i'', j')
      in
      let bottom_left =
        let i'', j'' = i' / 2 + 1, j' / 2 in
        region_sum rect (i'', j) (i', j'')
      in
      let bottom_right =
        let i'', j'' = i' / 2 + 1, j' / 2 + 1 in
        region_sum rect (i'', j'') (i', j')
      in
      top_left + top_right + bottom_left + bottom_right
end
