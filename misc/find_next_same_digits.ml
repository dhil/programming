(* Given a number n, find the largest number just smaller than n that
   can be formed using the same digits as n. *)

(* Variant: Find next greater number with same set of digits *)
module Array = struct
  include Array

  let rev_iteri f arr =
    let ulimit = Array.length arr - 1 in
    for i = ulimit downto 0 do
      f i arr.(i)
    done

  let rev src =
    let n = Array.length src in
    Array.init n (fun i -> src.(n - i - 1))
end

module Nat: sig
  type t = int array

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
end = struct
  type t = int array

  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
       let b = pow a (n / 2) in
       b * b * (if n mod 2 = 0 then 1 else a)

  let rec niter f x n =
    if n <= 0
    then x
    else niter f (f x) (n-1)

  let of_int n =
    assert (n >= 0);
    let size = String.length (Printf.sprintf "%d" n) in
    let ndiv x n =
      niter (fun x -> x / 10) x n
    in
    let make_ith i =
      (ndiv n i) mod 10
    in
    Array.init size make_ith

  let to_int n =
    let n' =
      Array.mapi (fun i d -> d * (pow 10 i)) n
    in
    Array.fold_left (fun sum n -> n + sum) 0 n'

  let to_string n =
    let size = Array.length n in
    let get_ith i =
      let s = string_of_int (n.(size - i - 1)) in
      s.[0]
    in
    let bytes =
      Bytes.init size get_ith
    in
    Bytes.to_string bytes
end

let find_next_greater n =
  let exception Break in
  let num = Nat.of_int n in
  let i = ref (Array.length num - 1) in
  (try
     for j = !i downto 1 do
       if num.(j) < num.(j-1)
       then (i := j; print_int j; print_endline ""; raise Break)
       else ()
     done
   with Break -> ());
  if !i = 0 then None
  else
    let x = num.(!i - 1) in
    let smallest = ref !i in
    for j = !i + 1 to Array.length num - 1 do
      if num.(j) > x && num.(j) < num.(!smallest)
      then smallest := j
      else ()
    done;
    (* swap *)
    let swap a i j =
      let tmp = num.(i) in
      num.(i) <- num.(j);
      num.(j) <- tmp
    in
    swap num !smallest (!i - 1);
    (* sort [!i, len] *)
    for j = !i to Array.length num - 1 do
      if j+1 < Array.length num && num.(j) > num.(j+1) then
        swap num j (j+1)
    done;
    Some (Nat.to_int num)

