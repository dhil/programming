(* There is an array of non-negative integers. A second array is
   formed by shuffling the elements of the first array and deleting a
   random element. Given these two arrays, find which element is missing
   in the second array. *)

(* ... in other words this task asks to compute the "set difference"
   on arrays. *)

(* This solution is more general. O(n). *)
let diff : int array -> int array -> int array
  = fun a b ->
  let set = Hashtbl.create (Array.length b) in
  Array.iter
    (fun x ->
      if Hashtbl.mem set x then ()
      else Hashtbl.add set x ())
    b;
  let dset = Hashtbl.create (Array.length a) in
  Array.iter
    (fun x ->
      if Hashtbl.mem set x then ()
      else Hashtbl.add dset x ())
    a;
  let darr =
    let length = Hashtbl.length dset in
    Array.make length 0
  in
  let (a, _) =
    Hashtbl.fold
      (fun x _ (a,i) ->
        a.(i) <- x;
        (a, i+1))
      dset (darr, 0)
  in a

(* Specialised solution. O(n). *)
let diff' : int array -> int array -> int
  = fun a b ->
    Array.fold_left
      (fun acc x ->
        acc lxor x)
      0 (Array.append a b)
