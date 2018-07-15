(* Code adapted from "What is a purely functional?" by Hofmann et al. (2010) *)

type tree =
  | Answer of int
  | Question of int * (int -> tree)

let rec tree2fun tr k =
  match tr with
  | Answer c -> c
  | Question (a, f) -> tree2fun (f (k a)) k

let called = ref false
let arg = ref 0
let questions = ref []
let answers = ref []
let init ds =
  called := false;
  arg := 0;
  questions := [];
  answers := ds

let ktest a =
  if !called then 0
  else match !answers with
  | [] -> (called := true; arg := a; 0)
  | x :: xs ->
    questions := !questions @ [a];
    answers := xs;
    x

(* The inductive definition fun2tree_aux represents the well-founded
   part of the graph *)
let rec fun2tree_aux ff ds =
  init ds;
  let result = ff ktest in
  if not !called then Answer result
  else let a0 = !arg in
       Question (a0, fun b -> fun2tree_aux ff (ds @ [b]))

let fun2tree ff = fun2tree_aux ff []

let example =
  let q = fun2tree (fun k -> 1 + k 0) in
  match q with
  | Question (0, f) ->
     begin match f 1, (tree2fun q) (fun i -> i + 1) with
     | Answer 2, 2 -> true
     | _ -> assert false
     end
  | _ -> assert false
