(* All Permutations of String *)

let rec fact n = if n = 0 then 1 else n * fact (n-1)

(* O(n!) *)
let permute : 'a array -> 'a array list
  = fun a ->
    let swap a i j =
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
    in
    let rec permute a l u acc =
      if l = u then
        Array.copy a :: acc (* Copying is O(n) *)
      else
        let rec until i u acc =
          if i <= u then
            begin
              swap a l i;
              (* Permute substring *)
              let acc = permute a (l+1) u acc in
              swap a l i;
              until (i+1) u acc
            end
          else acc
        in
        until l u acc
    in
    permute a 0 (Array.length a - 1) []

let permute_string : string -> string list
  = fun s ->
    let array_of_string s =
      Array.init (String.length s) (fun i -> s.[i])
    in
    let string_of_array a =
      let bs = Bytes.init (Array.length a) (fun i -> a.(i)) in
      Bytes.to_string bs
    in
    let permutations = permute (array_of_string s) in
    List.map string_of_array permutations

