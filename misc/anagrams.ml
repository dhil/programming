(* Given two strings determine whether they are anagrams. *)

module String = struct
  include String

  let to_array s =
    Array.init
      (String.length s)
      (fun i -> s.[i])

  let of_array a =
    let bs =
      Bytes.init
        (Array.length a)
        (fun i -> a.(i))
    in
    Bytes.to_string bs
end

module HashMap = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t

  let mem tbl k =
    Hashtbl.mem tbl k

  let put tbl k v =
    (if mem tbl k then
       Hashtbl.remove tbl k);
    Hashtbl.add tbl k v

  let get tbl k =
    Hashtbl.find tbl k

  let make capacity =
    Hashtbl.create capacity

  let fold f tbl acc =
    Hashtbl.fold f tbl acc
end

let anagram s1 s2 =
  let a1, a2 =
    String.to_array s1, String.to_array s2
  in
  let dict =
    HashMap.make (String.length s1)
  in
  let rec loop f i n =
    if i < n
    then (f i; loop f (i+1) n)
    else ()
  in
  let increment i =
    if HashMap.mem dict a1.(i)
    then
      let count = HashMap.get dict a1.(i) in
      HashMap.put dict a1.(i) (count + 1)
    else
      HashMap.put dict a1.(i) 1
  in
  loop increment 0 (Array.length a1);
  let exception Not_Anagram in
  let decrement i =
    if HashMap.mem dict a2.(i)
    then
      let count = HashMap.get dict a2.(i) in
      HashMap.put dict a2.(i) (count - 1)
    else
      raise Not_Anagram
  in
  try
    loop decrement 0 (Array.length a2);
    HashMap.fold
      (fun c count acc ->
        count = 0 && acc)
      dict true
  with Not_Anagram -> false
