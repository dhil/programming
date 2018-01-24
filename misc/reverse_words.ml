(* Given an input string, reverse all the words. *)

let reverse_words sentence =
  let sentence = String.trim sentence in
  let rec words s len i j acc =
    if i > len then acc
    else if s.[i]
