(* First task: String matching.

   is_match p s = true if the pattern p matches the string s

   A pattern can contain any symbol. Numbers are interpreted
   specially, e.g. i18n matches a string that start with i, 18
   arbitrary symbols, and the ends in an n such as
   "internationalization".

   A string can contain any symbol except for digits.
 *)

module List = struct
  include List

  let of_string : string -> char list
    = fun s ->
    let rec loop s i =
      if i < String.length s
      then String.get s i :: loop s (i+1)
      else []
    in
    loop s 0
end

let is_digit : char -> bool
  = fun ch ->   Char.code '0' <= Char.code ch
             && Char.code ch  <= Char.code '9'

let parse_number : char list -> int * char list
  = fun cs ->
  let rec parse = function
    | [] ->
       1, 0, []
    | c :: cs when not (is_digit c) ->
       1, 0, c :: cs
    | c :: cs ->
       let (n, sum, cs') = parse cs in
       let i = int_of_string (Printf.sprintf "%c" c) in
       (n * 10, sum + i * n, cs')
  in
  let (_, n, cs') = parse cs in
  (n, cs')

(* Time complexity: O(|s|) *)
let is_match : string -> string -> bool
  = fun pat s ->
  let rec is_match : char list -> char list -> bool
    = fun ps cs ->
    match ps, cs with
    | [], _ -> true
    | _ :: _, [] -> false
    | p :: ps, c :: cs when not (is_digit p) ->
       p = c && is_match ps cs
    | ps, cs ->
       let rec take : int -> char list -> char list option
         = fun n cs ->
         if n = 0 then Some cs
         else match cs with
              | [] -> None
              | _ :: cs' -> take (n - 1) cs'
       in
       let (n, ps') = parse_number ps in
       match take n cs with
       | None -> false
       | Some cs' -> is_match ps' cs'
  in
  is_match (List.of_string pat) (List.of_string s)

let examples () =
  let inputs = [("i18n", "internationalization"); ("F2eb2k", "Facebook"); ("F2eb2k", "Focus"); ("a2a", "baab")] in
  List.map (fun (p, s) -> (p, s, is_match p s)) inputs

(* Second task: Arithmetic expression evaluation.

   calculate s = n, where s is a string representation of an
                    arithmetic expression. The expressions are restricted to
                    non-negative integers and will use + and * operators. *)
let calculate : string -> int
  = fun s ->
  let css =
    let (css, cs) =
      List.fold_right
        (fun c (css, cs) ->
          match c with
          | '+' -> cs :: css, []
          | _   -> css, c :: cs)
        (List.of_string s) ([], [])
    in
    cs :: css
  in
  let rec parse_and_drop_mult cs =
    match parse_number cs with
    | (n, []) -> [n]
    | (n, '*' :: cs') -> n :: parse_and_drop_mult cs'
    | _ -> assert false
  in
  let nss = List.map parse_and_drop_mult css in
  let nss' = List.map (fun ns -> List.fold_left ( *) 1 ns) nss in
  List.fold_left (+) 0 nss'

let examples' () =
  let inputs = ["3+2*4+5"; "3+2*4*5"; "3+2+4+5"] in
  List.map (fun s -> (s, calculate s)) inputs
