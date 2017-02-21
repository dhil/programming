(* A modified version of test/good/interpreter.f, with useless
   match clauses added. *)

(* This file presents a tagless interpreter for the simply-typed
   lambda-calculus. This is a classic example of use of GADTs. *)

(* The unit type. *)

type unit

data constructor Unit : {} -> unit

(* The type constructors [nil] and [cons] are used to encode type-level
   lists. These lists are used to encode typing environments. *)

type nil

type cons e a

(* The algebraic data type [var e a] is used to encode variables
   that have type [a] under the typing environment [e]. The data
   constructors of this type are [Z] and [S], zero and successor:
   a variable is represented as an integer -- a de Bruijn index --
   in unary notation. *)

type var e a

data constructor Z : forall e a. {} -> var (cons e a) a

data constructor S : forall e b a. { var e a } -> var (cons e b) a

(* The algebraic data type [term e a] is used to encode lambda
   terms that have type [a] under the typing environment [e].
   The data constructors are [Var], [Lam], and [App], which
   correspond to variables, abstractions, and applications.
   The types ascribed to these data constructors reflect the
   typing rules of the simply-typed lambda-calculus. *)

type term e a

data constructor Var : forall e a. { var e a } -> term e a

data constructor Lam : forall e a b. { term (cons e a) b } -> term e (a -> b)

data constructor App : forall e a b. { term e (a -> b); term e a } -> term e b

(* The algebraic data type [env e] is used to encode runtime
   environments -- that is, lists of values -- that correspond
   to the typing environment [e]. *)

type env e

data constructor EnvNil : {} -> env nil

data constructor EnvCons : forall e a. { env e; a } -> env (cons e a)

(* Here is the code. *)

program

(* A diverging function, used to provide some code in the useless
   clauses. *)

let rec loop [ a ] (u : unit) : a = loop [ a ] u in

(* The function [lookup] looks up the value associated with the
   variable [x] in the runtime environment [env]. In short, [x]
   is an integer in unary notation, [env] is a list, and [lookup]
   retrieves the [x]-th element in the list [env]. *)

(* Note how the type-checker is able to prove that [env] cannot
   be [EnvNil], that is, the lookup never runs off the end of
   the environment -- the integer [x] is always within bounds. *)

let rec lookup [ e ] [ a ] (env : env e) (x : var e a) : a =
  match x return a with
  | Z [ _ ] [ _ ] {} ->
      match env return a with
      | EnvNil {} ->
          loop [ a ] (Unit {})
      | EnvCons [ _ ] [ _ ] { _; v } ->
          v
      end
  | S [ e ] [ _ ] [ _ ] { x } ->
      match env return a with
      | EnvNil {} ->
          loop [ a ] (Unit {})
      | EnvCons [ _ ] [ _ ] { env; _ } ->
          lookup [ e ] [ a ] env x
      end
  end
in

(* The function [interpret] interprets the term [t] under the
   runtime environment [env]. If the term [t] has type [a] in
   the object language, then its interpretation produces a
   meta-language value of type [a]. *)

(* A variable is interpreted by looking it up in the environment.
   An abstraction is interpreted as a meta-language closure which
   captures the current environment [env] and, when invoked with
   an argument [v], extends [env] with [v] and inteprets the body
   of the abstraction. An application is interpreted by interpreting
   both the function and its argument, and applying the value of the
   former to the value of the latter. Note how the type-checker
   guarantees that the result of interpreting an objet-level
   function is a meta-level function. No runtime check is
   necessary to check that the application of [interpret] to [t1]
   returns a function. *)

let rec interpret [ e ] [ a ] (env : env e) (t : term e a) : a =
  match t return a with
  | Var [ _ ] [ _ ] { x } ->
      lookup [ e ] [ a ] env x
  | Lam [ _ ] [ domain ] [ codomain ] { t } ->
      fun (v : domain) = interpret [ cons e domain ] [ codomain ] (EnvCons [ e ] [ domain ] { env; v }) t
  | App [ _ ] [ domain ] [ _ ] { t1; t2 } ->
      interpret [ e ] [ domain -> a ] env t1 (interpret [ e ] [ domain ] env t2)
  end
in

(* An example: an object-level representation of the identity. *)

let identity [ a ] : term nil (a -> a) =
  Lam [ nil ] [ a ] [ a ] { Var [ cons nil a ] [ a ] { Z [ nil ] [ a ] {} } }
in

(* An example: interpret the object-level identity in an empty runtime
   environment, to obtain the meta-language identity. *)

(* This program has type forall a. a -> a. *)

fun [ a ] =
  interpret [ nil ] [ a -> a ] (EnvNil {}) (identity [ a ])

