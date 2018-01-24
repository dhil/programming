(* Given an input string, reverse all the words. *)

let reverse_words sentence =
  let sentence = String.trim sentence in
  let ws_code = Char.code ' ' in
  let rec words s len i j acc =
    if j = len then
      String.sub s i (j - i) :: acc
    else if Char.code s.[j] = ws_code then
      let acc =
        if i <> j then
          String.sub s i (j - i) :: acc
        else
          acc
      in
      words s len (j+1) (j+1) acc
    else
      words s len i (j+1) acc
  in
  words sentence (String.length sentence) 0 0 []
