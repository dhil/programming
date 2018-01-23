(* Given an integer array, output all pairs that sum up to a specific
   value k. *)

module HashSet = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t

  let mem tbl k = (* O(1) *)
    Hashtbl.mem tbl k

  let put tbl k v = (* O(1) *)
    (if mem tbl k then
        Hashtbl.remove tbl k);
    Hashtbl.add tbl k v

  let get tbl k = (* O(1) *)
    Hashtbl.find tbl k

  let make size =
    Hashtbl.create size

  let fold f tbl zero
      = Hashtbl.fold f tbl zero
end

(* O(|arr|) *)
let pair_sums : int -> int array -> (int * int) list
  = fun k arr ->
    let seen =
      HashSet.make (Array.length arr)
    in
    Array.fold_left
      (fun sums i ->
        let open HashSet in
        let n = k - i in
        if not (mem seen n) then
          (put seen n true; sums)
        else
          (min i n, max i n) :: sums)
      [] arr

(* O(|arr|) *)
let pair_sums_distinct : int -> int array -> (int * int) list
  = fun k arr ->
    let seen =
      HashSet.make (Array.length arr)
    in
    let pairs =
      HashSet.make (Array.length arr)
    in
    Array.iter
      (fun i ->
        let open HashSet in
        let n = k - i in
        if not (mem seen n) then
          put seen i true
        else
          put pairs i (min i n, max i n))
      arr;
    HashSet.fold
      (fun _ p pairs ->
        p :: pairs)
      pairs []
