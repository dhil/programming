(* Given a string determine whether it is periodic. *)

(* Definition: A string `s` is said to have period `k > 2` if forall `0 <=
   i < |n|/k` it holds that `s[i] = s[i + k]`. *)

(* Tests whether a string is `k` periodic in O(|s|) time. *)
let is_k_periodic : int -> string -> bool
  = fun k s ->
  assert (k > 0);
  if String.length s > k && String.length s mod k = 0
  then let rec loop s k i =
         if i + k < String.length s
         then if Char.equal s.[i] s.[i + k]
              then loop s k (i + 1)
              else false
         else true
       in
       loop s k 0
  else false

(* Determines whether a string is periodic in O(|s|) time. *)
let is_periodic : string -> bool
  = fun s ->
  (* Attempt to guess the period `k` using chasing pointers in
     O(|s|)-time. *)
  let rec guess_k s i k =
    if i + k < String.length s
    then if Char.equal s.[i] s.[i + k]
         then guess_k s (i + 1) k
         else guess_k s i (k + 1)
    else k
  in
  let k = guess_k s 0 1 in (* `k` must be at least 1. *)
  is_k_periodic k s

let examples =
  List.map
    (fun (s, expect) -> (s, expect, is_periodic s))
    [ "", false
    ; "a", false
    ; "aa", true
    ; "ab", false
    ; "abab", true
    ; "abba", false
    ; "abcdabcdabcd", true
    ; "aaaaaaaf", false
    ; "ababa", false
    ; "ababababc", false
    ; "ababababab", true
    ; "aabaabaaaabaabaaaabaabaa", true
    ]
