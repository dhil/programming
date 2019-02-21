(* An implementation of "On Evaluation Contexts, Continuations, and
   the Rest of the Computation" (Danvy, 2004). *)

(* Arithmetic expressions *)
module Aexp = struct
  (* Minimal expression language: literals are the only values, whilst
     additions are the only computations. *)
  type exp = Value of value
           | Comp of comp
  and value = Lit of int
  and comp = Add of exp * exp

  module Plug = struct
    (* One-step reduction function *)
    let rec reduce1 : comp -> exp = function
      | Add (Value (Lit a), Value (Lit b)) ->
         Value (Lit (a + b))
      | Add (Value v, Comp c) ->
         Comp (Add (Value v, reduce1 c))
      | Add (Comp c, e) ->
         Comp (Add (reduce1 c, e))

    (* CPS transformed [reduce1] *)
    let rec reduce1c : comp -> (exp -> 'a) -> 'a
      = fun comp k ->
      match comp with
      | Add (Value (Lit a), Value (Lit b)) ->
         k (Value (Lit (a + b)))
      | Add (Value v, Comp c) ->
         reduce1c c (fun e' -> k (Comp (Add (Value v, e'))))
      | Add (Comp c, e) ->
         reduce1c c (fun e' -> k (Comp (Add (e', e))))

    let rec main : exp -> int = function
      | Value (Lit n) -> n
      | Comp comp ->
         let e' = reduce1 comp in
         main e'

    let rec mainc : exp -> int = function
      | Value (Lit n) -> n
      | Comp comp ->
         let e' = reduce1c comp (fun e -> e) in
         mainc e'

    (* Defunctionalistion *)
    module Defunct = struct
      type cont = Cont0
                | Cont1 of value * cont
                | Cont2 of exp * cont

      let rec apply : cont -> exp -> exp
        = fun k e ->
        match k with
        | Cont0 -> e
        | Cont1 (v, k) ->
           apply k (Comp (Add (Value v, e)))
        | Cont2 (e', k) ->
           apply k (Comp (Add (e', e)))

      let rec reduce1cd : comp -> cont -> exp
        = fun comp k ->
        match comp with
        | Add (Value (Lit a), Value (Lit b)) ->
           apply k (Value (Lit (a + b)))
        | Add (Value v, Comp c) ->
           reduce1cd c (Cont1 (v, k))
        | Add (Comp c, e) ->
           reduce1cd c (Cont2 (e, k))

      let rec main : exp -> int = function
        | Value (Lit n) -> n
        | Comp comp ->
           let e' = reduce1cd comp Cont0 in
           main e'
    end
  end

  module Refocus = struct

    let rec eval : exp -> int = function
      | Value (Lit n) -> n
      | Comp (Add (e, e')) ->
         let lhs = eval e in
         let rhs = eval e' in
         lhs + rhs

    let main : exp -> int = eval

    (* CPS transformed [eval] *)
    let rec evalc : exp -> (int -> 'a) -> 'a
      = fun e k ->
      match e with
      | Value (Lit n) -> k n
      | Comp (Add (e, e')) ->
         evalc e (fun a -> evalc e' (fun b -> k (a + b)))

    let mainc : exp -> int
      = fun e -> evalc e (fun a -> a)

    (* Defunctionalisation *)
    module Defunct = struct
      type cont = Cont0
                | Cont1 of int * cont
                | Cont2 of exp * cont

      let rec apply : cont -> int -> int
        = fun k n ->
        match k with
        | Cont0 -> n
        | Cont1 (n', k) ->
           apply k (n' + n)
        | Cont2 (e, k) ->
           evalcd e (Cont1 (n, k))
      and evalcd : exp -> cont -> int
        = fun e k ->
        match e with
        | Value (Lit n) ->
           apply k n
        | Comp (Add (e, e')) ->
           evalcd e (Cont2 (e', k))

      let main : exp -> int
        = fun e -> evalcd e Cont0
    end
  end

  let example =
    (* 40 + 1 + 1 *)
    Comp
      (Add
         (Value (Lit 1),
          Comp
            (Add
               (Value (Lit 40),
                Value (Lit 1)))))

  let run () =
    let evals = [("Aexp.Plug.main", Plug.main);
                 ("Aexp.Plug.mainc", Plug.mainc);
                 ("Aexp.Plug.Defunct.main", Plug.Defunct.main);
                 ("Aexp.Refocus.main", Refocus.main);
                 ("Aexp.Refocus.mainc", Refocus.main);
                 ("Aexp.Refocus.Defunct.main", Refocus.Defunct.main)]
    in
    let results = List.map (fun (lbl, eval) -> (lbl, eval example)) evals in
    List.iter
      (fun (lbl, result) -> Printf.printf "%25s (1 + 40 + 1) = %d\n%!" lbl result) results
end
