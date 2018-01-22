(* Large naturals *)

module BigNat: sig
  type t = int array

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
  val add : t -> t -> t
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

  let add a b =
    let (a, b) =
      if Array.length a <= Array.length b
      then (a, b)
      else (b, a)
    in
    let size =
      if a.(Array.length a - 1) + b.(Array.length b - 1) > 9
      then Array.length b + 1
      else Array.length b
    in
    let carry = ref 0 in
    let add_ith i =
      print_int i; print_endline "";
      let add carry x y =
        let n = carry + x + y in
        (n mod 10, n / 10)
      in
      let ai = if i < Array.length a then a.(i) else 0 in
      let (digit, carry') = add !carry ai b.(i) in
      carry := carry';
      digit
    in
    Array.init size add_ith
end
