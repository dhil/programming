(* Given dictionary and a string with possible wildcard(s) search the
   dictionary for whether there is a match for the given string. *)

module CharMap = Map.Make(struct type t = char let compare = Char.compare end)

let explode : string -> char list
  = fun s ->
  let rec loop i s =
    if i < String.length s
    then s.[i] :: loop (i + 1) s
    else []
  in loop 0 s

module Dictionary = struct
  type t = Dict of t CharMap.t [@@unboxed]

  let empty : t = Dict (CharMap.empty)

  let rec add : char list -> t -> t
    = fun word (Dict dict) ->
    match word with
    | [] -> Dict dict
    | c :: cs ->
       Dict (try
             let dict' = CharMap.find c dict in
             CharMap.add c (add cs dict') dict
           with Not_found ->
             CharMap.add c (add cs (Dict CharMap.empty)) dict)

  let rec exists : char list -> t -> bool
    = fun word (Dict dict) ->
    match word with
    | [] -> true
    | '*' :: cs ->
       CharMap.exists (fun _ dict' -> exists cs dict') dict
    | c :: cs ->
       try
         let dict' = CharMap.find c dict in
         exists cs dict'
       with Not_found -> false
end

let sample_dict =
  Dictionary.(add (explode "foobar")
                (add (explode "baz")
                   (add (explode "abba")
                      (add (explode "akka")
                         (add (explode "quux") empty)))))

let test_inputs =
  ["*"; ""; "foobar"; "fo*bar"; "f**bar"; "foo***"; "bar"; "a**a"; "quu*"; "word"; "wee"; "foo*az"]

let tests =
  List.map (fun s -> (s, Dictionary.exists (explode s) sample_dict)) test_inputs
