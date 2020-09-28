
(* Dan Grossman; Graduate Programming Languages; Lecture 14 *)

(* disclaimers about the code:
   1. Code has not been thoroughly tested
   2. Code assumes there are no free variables in the input program
   3. The capture-avoiding substitution is brutally inefficient; there are
      much better ways
   4. We assume in a few places that the only values are lambdas, which
      is true for our tiniest calculus
 *)

(* definition of syntax and substitution for the basic lambda calculus *)

type exp = 
    V of string
  | L of string * exp
  | A of exp * exp

let new_string =
  let i = ref 0 in
  (fun () -> i := (!i)+1; "__" ^ (string_of_int !i))

let rec rename old_s new_s e =
  let r = rename old_s new_s in
  match e with
    V s -> if old_s=s then V new_s else e
  | L (s,e') -> if s=old_s then e else L(s, r e')
  | A (e1,e2) -> A(r e1, r e2)

let rec substitute e_for s in_e =
  let r = substitute e_for s in
  match in_e with
    V s2 -> if s2=s then e_for else in_e
  | L (s,e') -> 
      let new_s = new_string() in 
      L(new_s, r (rename s new_s e'))
  | A(e1,e2) -> A(r e1, r e2)

(* version 1: plain-old small-step semantics *)

(* at each step, interp_one uses recursion to decide where to take a
   primitive step *)

let rec interp_one e =
  match e with
    V _ -> failwith "interp_one"
  | L _ -> failwith "interp_one"
  | A(L(s1,e1),L(s2,e2)) -> substitute (L(s2,e2)) s1 e1
  | A(L(s1,e1),e2)       -> A(L(s1,e1),interp_one e2)
  | A(e1,e2)             -> A(interp_one e1, e2)

let rec interp_small e =
  match e with
    V _ -> failwith "interp_small"
  | L _ -> e
  | A(_,_) -> interp_small (interp_one e)

(* definition of evaluation contexts and hole-filling *)

type eval_context =
    Hole
  | Left of eval_context * exp
  | Right of exp * eval_context (* exp should actually be a value *)

let rec fill_with_exp c e =
  match c with
    Hole -> e
  | Left(c2,e2)  -> A(fill_with_exp c2 e, e2)
  | Right(e2,c2) -> A(e2, fill_with_exp c2 e)

(* version 2: decompose at each step *)

(* at each step, we explicitly (a) decompose, (b) take a primitive step, and
   (c) put the pieces back together *)

(* this version makes it much easier to add Letcc and Throw
   (and much easier than a large-step semantics, which is what you could
   use if you didn't want continuations or threads or ...)
 *)

let rec decompose e =
  match e with
    V _ -> failwith "decompose"
  | L _ -> failwith "decompose"
  | A(L(s1,e1),L(s2,e2)) -> (Hole,e)
  | A(L(s1,e1),e2) -> 
      let (c,e3) = decompose e2 in
      (Right(L(s1,e1),c), e3)
  | A(e1,e2) ->
      let (c,e3) = decompose e1 in
      (Left(c,e2),e3)

let rec interp_evalcontext e =
  match e with
    V _ -> failwith "interp_evalcontext"
  | L _ -> e
  | _ -> 
      let (ctxt,e_active) = decompose e in
      match e_active with
	A(L(s1,e1),L(s2,e2)) -> 
	  let e_result   = substitute (L(s2,e2)) s1 e1 in
	  let e_new_prog = fill_with_exp ctxt e_result in
	  interp_evalcontext e_new_prog
      |	_ -> failwith "interp_evalcontext"

(* how to do continuations if our language had them:
   (0) Extend decompose with two new cases (subexpressions of Throw)
   (1) Cont E is a value (so return it like we return lambdas)
   (2) additional cases in inner match
     | Letcc(x,e) -> 
           let e_result = A(L(x,e),Cont ctxt) in
           let e_new_prog = fill_with_exp ctxt e_result in
           interp_evalcontext e_new_prog
     | Throw(Cont ctxt2, v) ->
           let e_new_prog = fill_with_exp ctxt2 v in
           interp_evalcontext e_new_prog
   Note the Throw case "throws away" ctxt
*)

(* version 3: use a stack instead of "nested holes" to avoid re-decomposing *)

(* re-decomposing at each step is unrealistically inefficient, and unnecessary.
   This is the toughest step though: how to maintain an explicit stack that
   represents the evaluation context *)

type stack_context_elt = 
    SLeft of exp
  | SRight of exp (* exp should actually be a value *)
type stack_context = stack_context_elt list
(* shallow end of stack at beginning of list *)

(* we do not need these next three functions; they are here just to convince
   us that stack_context and eval_context are isomorphic! *)

let eval_context_to_stack ctxt =
  let rec r ctxt =
    match ctxt with
      Hole -> []
    | Left(c2,e)  -> SLeft(e)::(r c2)
    | Right(e,c2) -> SRight(e)::(r c2)
  in List.rev(r ctxt)

let rec fill_with_context c1 c2 =
  let r = fill_with_context c1 in
  match c2 with
    Hole -> c1
  | Left(c3,e)  -> Left(r c3, e)
  | Right(e,c3) -> Right(e, r c3)

let rec stack_to_eval_context stack =
  match stack with
    [] -> Hole
  | (SLeft(e))::tl -> 
      fill_with_context (Left(Hole,e))  (stack_to_eval_context tl)
  | (SRight(e))::tl ->
      fill_with_context (Right(e,Hole)) (stack_to_eval_context tl)

(* the actual interpreter: except for substitution we're not using
   Caml's recursion anymore (except for a while loop) *)

let interp_stack e =
  let rec loop c e =
    match e with
      (* variables should be substituted away for *)
      V _ -> failwith "interp_stack"

      (* start an application by working on left (pushing the right) *)
    | A(e1,e2) -> loop (SLeft(e2)::c) e1

      (* else e is a value *)
    | L _ -> 
	match c with

        (* nothing on my stack, the whole program is a value *)
	| [] -> e

       (* I was working on the left of an application; now
        work on the right (change the context) *)
	| (SLeft e_right)::tl -> loop (SRight(e)::tl) e_right

       (* I was working on the right, now do the substitution.
          This shrinks the context by one.  Subsequent steps will grow the
          context if the result of the substitution is a complex expression,
          or do more shrinking or changing if it's a value. *)
        | (SRight(L(s1,e1)))::tl -> loop tl (substitute e s1 e1)

        (* impossible case: SRight always carries a lambda *)
        | (SRight _)::_ -> failwith "interp_stack"
  in
  loop [] e

(* version 4: use environments instead of substitution *)

(* This gets rid of our last use of recursion and unreasonable overhead.
   The idea of using closures is the same as in hw3: maintain an environment,
   store environments with function-values (called closures), and 
   lookup variables in environments.
*)

type env = (string * exp2) list (* map variables to values *)
and exp2 = 
    V2 of string
  | L2 of string * exp2
  | A2 of exp2 * exp2
  | Closure of string * exp2 * env (* lambda plus environment *)
(* note that in every env we build, every exp2 will actually be a Closure
   because those are our values *)

let rec exp_to_exp2 e =
  match e with
    V s      -> V2 s
  | L(s,e)   -> L2(s,exp_to_exp2 e)
  | A(e1,e2) -> A2(exp_to_exp2 e1, exp_to_exp2 e2)

type stack_context_elt2 = 
    SLeft2 of exp2 * env (* remember environment for right-hand side *)
  | SRight2 of exp2 (* exp should actually be a value (a closure) *)
type stack_context2 = stack_context_elt2 list

let rec lookup env s =
  match env with
    [] -> failwith "lookup"
  | ((s2,closure)::tl) -> if s2=s then closure else lookup tl s

let interp_closure e =
  let rec loop c env e =
    match e with

     (* variables now looked up as needed in environment.
        result will be a value; so next time around loop will use bottom
        branch *)
     V2 s -> loop c env (lookup env s)

    (* start an application by working on left (pushing the right) *)
    (* must save the current environment for later evaluation of e2 *)
    | A2(e1,e2) -> loop (SLeft2(e2,env)::c) env e1

    (* a lambda becomes a value by saving the current environment *)
    | L2(s1,e1) -> loop c env (Closure(s1,e1,env))

    (* else e is a value *)
    | Closure _ -> 
	match c with
	
         (* nothing on my stack, the whole program is a value *)
	| [] -> e

	(* I was working on the left of an application; now work on the
           right using the saved environment *)
	| (SLeft2(e_right,env2))::tl -> loop (SRight2(e)::tl) env2 e_right

        (* evaluate body under saved env1 extended to map s1 to 
           the value we just computed *)
	| (SRight2(Closure(s1,e1,env1)))::tl -> 
	    loop tl ((s1,e)::env1) e1 (* env1, not env! *)
	      
	| (SRight2 _)::_ -> failwith "interp_closure"
  in 
  loop [] [] (exp_to_exp2 e)

(* notice everything in interp_closure is tail-recursive (trivial to
translate to a while-loop) *)
