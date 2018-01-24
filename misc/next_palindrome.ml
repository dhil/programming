(* Given a number, find the next smallest palindrome larger than the number. *)

let is_palindrome n =
  let rec test s l u =
    l >= u || (s.[l] = s.[u] && test s (l+1) (u-1))
  in
  let s = Printf.sprintf "%d" n in
  test s 0 (String.length s - 1)

let rec next_palindrome_naive n =
  let n' = n + 1 in
  if is_palindrome n' then n'
  else next_palindrome_naive n'

let rec next_palindrome n =
  let bs = Bytes.of_string (Printf.sprintf "%d" n) in
  let rec mirror bs i j =
    if i < j then
      Bytes.(set bs j (get bs i); mirror bs (i+1) (j-1))
  in
  let round_up n t =
    let t = int_of_float (10.0 ** float_of_int (t-1)) in
    let f = t / 2 in
    ((n + f) / t) * t
  in
  let increment bs i =
    let open Char in
    let c = code (Bytes.get bs i) in
    let c' = if c + 1 > 57 then 48 else c + 1 in
    Bytes.set bs i (chr c')
  in
  let bs_length = Bytes.length bs in
  mirror bs 0 (bs_length - 1);
  let n' = int_of_string (Bytes.to_string bs) in
  if bs_length mod 2 = 0 then (* even *)
    begin
      if n' <= n
      then
        begin
          let mid = (bs_length - 1) / 2 in
          increment bs mid;
          increment bs (mid+1);
          let n' = int_of_string (Bytes.to_string bs) in
          if n' <= n
          then next_palindrome (round_up n bs_length)
          else n'
        end
      else n'
    end
  else (* odd *)
    begin
      if n' <= n
      then
        begin
          increment bs (bs_length / 2);
          let n' = int_of_string (Bytes.to_string bs) in
          if n' <= n
          then next_palindrome (round_up n bs_length)
          else n'
        end
      else n'
    end
