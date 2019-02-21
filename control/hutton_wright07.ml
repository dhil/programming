(* An ML implementation of "What is the meaning of these constant
   interruptions?" by Hutton and Wright (2007). *)

(* Signifies whether evaluation occurs in blocked or unblocked mode. *)
type status = B | U
let is_unblocked = function U -> true | _ -> false

(* Predicate which decides whether to fire an interruption. *)
let _ = Random.self_init ()
let interrupt status =
  if is_unblocked status then
    let bias = 0.1 in
    Random.float 1.0 < bias
  else false

module Expr = struct
  type t = Val of int
         | Throw
         | Add of t * t
         | Catch of t * t
         | Seqn of t * t
         | Block of t
         | Unblock of t

  let rec eval status expr =
    (* Determine whether to perform an interruption. *)
    if interrupt status
    then Throw
    else match expr with
         | Val n -> Val n
         | Throw -> Throw
         | Add (e, e') ->
            begin match eval status e with
            | Val i ->
               begin match eval status e' with
               | Val j -> Val (i + j)
               | Throw -> Throw
               | _ -> assert false
               end
            | Throw -> Throw
            | _ -> assert false
            end
         | Catch (e, e') ->
            begin match eval status e with
            | Val i -> Val i
            | Throw -> eval status e'
            | _ -> assert false
            end
         | Seqn (e, e') ->
            begin match eval status e with
            | Val _ -> eval status e'
            | Throw -> Throw
            | _ -> assert false
            end
         | Block e -> eval B e
         | Unblock e -> eval U e

  let int n = Val n
  let throw = Throw
  let add e e' = Add (e, e')
  let catch e e' = Catch (e, e')
  let seq e e' = Seqn (e, e')
  let block e = Block e
  let unblock e = Unblock e

  let rec to_string = function
    | Val n -> string_of_int n
    | Throw -> "throw"
    | Add (e, e') ->
       Printf.sprintf "(+ %s %s)" (to_string e) (to_string e')
    | Catch (e, e') ->
       Printf.sprintf "(catch %s %s)" (to_string e) (to_string e')
    | Seqn (e, e') ->
       Printf.sprintf "(seq %s %s)" (to_string e) (to_string e')
    | Block e -> Printf.sprintf "(block %s)" (to_string e)
    | Unblock e -> Printf.sprintf "(unblock %s)" (to_string e)

  (* Broken in the presence of asynchronous interrupts. *)
  let finally' x y = seq (catch x (seq y throw)) y

  let finally x y = block (seq (catch (unblock x) (seq y throw)) y)
end

module Machine = struct
  type stack = item list
  and item = Val of int
           | Han of code
           | Int of status
  and code = op list
  and op = Push of int
         | Pop
         | Throw
         | Add
         | Mark of code
         | Unmark
         | Set of status
         | Reset

  let exec ops status stack =
    (* Exception mode. *)
    let rec exec_exn status = function
      | Val _ :: stack ->
         exec_exn status stack
      | Int status' :: stack ->
         exec_exn status' stack
      | Han ops :: s ->
         exec ops status s
      | [] -> failwith "Unhandled exception."
    and exec ops status stack =
      (* Decide whether to fire an interruption. *)
      if interrupt status
      then exec_exn status stack
      else match ops, stack with
           | Push n :: ops, _ ->
              exec ops status (Val n :: stack)
           | Throw :: _, _ ->
              exec_exn status stack
           | Add :: ops, Val x :: Val y :: stack ->
              exec ops status (Val (x + y) :: stack)
           | Add :: _, _ -> failwith "Malformed stack."
           | Pop :: ops, Val _ :: stack ->
              exec ops status stack
           | Pop :: _, _ -> failwith "Malformed stack."
           | Mark ops' :: ops, _ ->
              exec ops status (Han ops' :: stack)
           | Unmark :: ops, x :: Han _ :: stack ->
              exec ops status (x :: stack)
           | Unmark :: _, _ -> failwith "Malformed stack."
           | Set status' :: ops, _ ->
              exec ops status' (Int status :: stack)
           | Reset :: ops, x :: Int status' :: stack ->
              exec ops status' (x :: stack)
           | Reset :: _, _ -> failwith "Malformed stack."
           | [], Val n :: _ -> n
           | [], _ -> failwith "segfault."
    in
    exec ops status stack
end

module Compiler = struct
  let rec compile : Expr.t -> Machine.code -> Machine.code
    = fun expr ops ->
    let open Machine in
    match expr with
    | Expr.Val n -> Push n :: ops
    | Expr.Throw -> Throw :: ops
    | Expr.Add (e, e') ->
       compile e (compile e' (Add :: ops))
    | Expr.Seqn (e, e') ->
       compile e (Pop :: compile e' ops)
    | Expr.Catch (e, e') ->
       Mark (compile e' ops) :: compile e (Unmark :: ops)
    | Expr.Block e ->
       Set B :: compile e (Reset :: ops)
    | Expr.Unblock e ->
       Set U :: compile e (Reset :: ops)
end

let example () =
  let prog =
    let open Expr in
    (catch
       (unblock
          (finally
             (add (int 2) (int 40))
             (int 0)))
       (int (-1)))
  in
  Machine.exec (Compiler.compile prog []) B []
