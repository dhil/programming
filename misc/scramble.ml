(* Scramble string [from https://leetcode.com/problems/scramble-string/]

We can scramble a string s to get a string t using the following algorithm:

    - If the length of the string is 1, stop.
    - If the length of the string is > 1, do the following:
       1. Split the string into two non-empty substrings at a random index,
          i.e., if the string is s, divide it to x and y where s = x + y.
       2. Randomly decide to swap the two substrings or to keep them in
          the same order. i.e., after this step, s may become s = x + y or s = y + x.
       3. Apply step 1 recursively on each of the two substrings x and y.

Given two strings s1 and s2 of the same length, return true if s2 is
a scrambled string of s1, otherwise, return false.

Example 1:

Input: s1 = "great", s2 = "rgeat"
Output: true
Explanation: One possible scenario applied on s1 is:
"great" --> "gr/eat" // divide at random index.
"gr/eat" --> "gr/eat" // random decision is not to swap the two substrings and keep them in order.
"gr/eat" --> "g/r / e/at" // apply the same algorithm recursively on both substrings. divide at ranom index each of them.
"g/r / e/at" --> "r/g / e/at" // random decision was to swap the first substring and to keep the second substring in the same order.
"r/g / e/at" --> "r/g / e/ a/t" // again apply the algorithm recursively, divide "at" to "a/t".
"r/g / e/ a/t" --> "r/g / e/ a/t" // random decision is to keep both substrings in the same order.
The algorithm stops now and the result string is "rgeat" which is s2.
As there is one possible scenario that led s1 to be scrambled to s2, we return true.

Example 2:

Input: s1 = "abcde", s2 = "caebd"
Output: false

Example 3:

Input: s1 = "a", s2 = "a"
Output: true


Constraints:

    s1.length == s2.length
    1 <= s1.length <= 30
    s1 and s2 consist of lower-case English letters.
 *)

(* Launch with ocaml -I `opam var lib`/multicont multicont.cma *)
module Control = struct
  open Effect
  type 'a eff += Choose : 'a list -> 'a eff

  let choose : 'a list -> 'a
    = fun xs -> perform (Choose xs)

  (* This function implements the algorithm outlined above. It makes
     use of `choose` to make "random" choices. *)
  let rec scramble : string -> string
    = fun s ->
    if String.length s = 1 then s
    else (* All possible ways to split `s`. *)
         let is = List.init (String.length s - 1) (fun i -> i+1) in
         (* Choose the correct index to split `s` at (if such an
            exists index). *)
         let i = choose is in
         (* Split at index `i` *)
         let x, y =
           String.sub s 0 i, String.sub s i (String.length s - i)
         in
         let x, y =
           (* Decides whether to swap. *)
           if choose [true; false] then
             (* Swap *)
             let x' = scramble y in
             let y' = scramble x in
             x', y'
           else
             (* No swap *)
             let x = scramble x in
             let y = scramble y in
             x, y
         in
         String.concat "" [x; y]

  (* This function instantiates the `Choose` with a backtracking
     semantics. *)
  let is_scramble : string -> string -> bool
    = fun s1 s2 ->
      let exception Match in
      let try_every_choice : (string, bool) Deep.handler
        = let open Deep in
          { retc = (fun s -> s = s2)
          ; exnc = (fun e -> raise e)
          ; effc = (fun (type a) (eff : a eff) ->
            match eff with
            | Choose xs ->
               Some
                 (fun (k : (a, _) continuation) ->
                   let open Multicont.Deep in
                   let r = promote k in
                   try
                     List.iter
                       (fun x ->
                         if resume r x then raise Match)
                       xs; false
                   with Match -> true)
            | _ -> None) }
      in
      Deep.match_with scramble s1 try_every_choice
end
