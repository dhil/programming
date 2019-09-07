(* Write a function to return if two words are exactly "one edit"
   away, where an edit is:

    - Inserting one character anywhere in the word (including at the
   beginning and end)
    - Removing one character
    - Replacing exactly one character
 *)

type tactic =
  | Insert
  | Remove
  | Replace

let tactics = [Insert; Remove; Replace]

let one_edit : string -> string -> bool
  = fun lhs rhs ->
  let rec loop i j lhs rhs tactics =
    match i < String.length lhs, j < String.length rhs with
    | true, true ->
       if Char.equal lhs.[i] rhs.[j]
       then loop (i + 1) (j + 1) lhs rhs tactics
       else apply i j lhs rhs tactics
    | false, true | true, false ->
       apply i j lhs rhs tactics
    | false, false ->
       match tactics with
       | [] -> true
       | _ -> false
  and apply i j lhs rhs = function
    | [] -> false
    | Insert :: tactics ->
       loop i (j + 1) lhs rhs [] || apply i j lhs rhs tactics
    | Replace :: tactics ->
       loop (i + 1) (j + 1) lhs rhs [] || apply i j lhs rhs tactics
    | Remove :: tactics ->
       loop (i + 1) j lhs rhs [] || apply i j lhs rhs tactics
  in
  loop 0 0 lhs rhs tactics


let words = ["foo"; "fo"; "oo"; "o"; "f"; ""; "bar"; "br"]

let rec permute_pairs = function
  | [] -> []
  | x :: xs ->
     List.map (fun y -> (x, y)) (x :: xs) :: permute_pairs xs
let _ =
  let pairs = List.concat (permute_pairs words) in
  let results =
    List.map (fun (lhs, rhs) ->
        (lhs, rhs, one_edit lhs rhs))
      pairs
  in
  List.sort (fun (lhs, rhs, bit) (lhs', rhs', bit') ->
      if bit && bit'
      then match String.compare lhs lhs', String.compare rhs rhs' with
           | 0, 0 -> 0
           | (-1), _ -> (-1)
           | _, y -> y
      else if bit then 1
      else (-1))
    results
