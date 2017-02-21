type bool
data constructor False : {} -> bool
data constructor True : {} -> bool

type int
data constructor O : {} -> int
data constructor S : {int} -> int

program

let one = S {O {}} in
let two = S {one} in

let eq [a] (x : a) (y : a) = True {} in

let or (x : bool) =
  match x return bool -> bool with
  | True {} -> fun (_ : bool) = True {}
  | False {} -> fun (y : bool) = y
  end
in    

let empty [a] (x : a) = False {} in

let insert [a] (x : a) (s : a -> bool) (y : a) =
  or (eq [a] x y) (s y)
in

insert [int] one (empty [int]) two

