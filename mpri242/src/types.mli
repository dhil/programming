(* This module defines the types of System F, as used internally by the
   typechecker. *)

open Atom

(* ------------------------------------------------------------------------- *)

(* Types. *)

(* We must adopt a representation of types that supports testing types for
   alpha-equivalence as well as abstracting and instantiating bound variables.

   Here, we choose a ``locally nameless'' representation, where free variables
   are represented as atoms and bound variables are represented as de Bruijn
   indices. This choice has the somewhat unpleasant consequence that there are
   two constructors for type variables, [TyBoundVar] and [TyFreeVar]. However,
   as far as the clients of this module are concerned, the former is non-existent:
   clients must never use it to construct a type; in return, clients can safely
   assume that it never occurs in a type.

   We distinguish between types and type contexts. A type context is a type with
   a hole. A type can be turned into a type context by replacing all occurrences
   of a certain type variable with holes. A type context is turned back into a
   type by filling all holes with a certain type. Type contexts are used in the
   representation of existential and universal types. *)

(* This representation is not particularly efficient. [fill] and [abstract]
   have linear time complexity. As a result, a simple traversal of a type has
   quadratic time complexity. This is acceptable here because we wish to
   emphasize simplicity and because our types should remain small. *)

(* For more details on the locally nameless representation of abstract syntax,
   see the paper ``Engineering Formal Metatheory'', by Brian Aydemir, Arthur
   Charguéraud, Benjamin C. Pierce, Randy Pollack and Stephanie Weirich, in
   POPL 2008. *)

(* As in the paper ``Polymorphic typed defunctionalization and concretization'',
   types include type variables, function types, universal types, and algebraic
   data types. In the paper, data constructors receive type schemes, which are
   not considered types: type schemes include type equations and tuple (record)
   types. Here, for greater simplicity and uniformity, we enlarge the syntax of
   types, so that type schemes can be considered types. *)

type ftype =
  | TyBoundVar of int
      (* this constructor must not be used by clients *)
  | TyFreeVar of atom
      (* a *)
  | TyArrow of ftype * ftype
      (* T -> T *)
  | TyForall of ftype_context
      (* forall a . T *)
  | TyCon of atom * ftype list
      (* tc T ... T *)
  | TyTuple of ftype list
      (* { T; ... T } *)
  | TyWhere of ftype * ftype * ftype
      (* T where T = T *)

and ftype_context
      (* the representation of type contexts is not published, so the only way
	 to create and use them is via [abstract] and [fill] below. *)

(* ------------------------------------------------------------------------- *)

(* [equal] tells whether two types are equal up to alpha-equivalence. *)

val equal: ftype -> ftype -> bool

(* ------------------------------------------------------------------------- *)

(* [occurs p ty] tells whether a type variable [a] that satisfies the predicate
   [p] occurs free in the type [ty]. *)

val occurs: (atom -> bool) -> ftype -> bool

(* ------------------------------------------------------------------------- *)

(* [ftv ty] returns the free type variables of the type [ty]. *)

val ftv: ftype -> AtomSet.t

(* ------------------------------------------------------------------------- *)

(* [abstract a ty] replaces every free occurrence of the type variable
   [a] in the type [ty] with a hole, producing a type context. *)

val abstract: atom -> ftype -> ftype_context

(* [fill c ty] fills the type context [c] with the type [ty], producing a
   type. *)

val fill: ftype_context -> ftype -> ftype

(* ------------------------------------------------------------------------- *)

(* [hint c] suggests an identifier to represent the hole in the type
   context [c]. *)

val hint: ftype_context -> Identifier.identifier

(* ------------------------------------------------------------------------- *)

(* Typing environments map term variables to types. *)

type tenv

val empty: tenv
val lookup: atom -> tenv -> ftype
val bind: atom -> ftype -> tenv -> tenv
val binds: (atom * ftype) list -> tenv -> tenv

(* ------------------------------------------------------------------------- *)

(* A conjunction of type equations is represented as a list. *)

type equations =
    (ftype * ftype) list

(* ------------------------------------------------------------------------- *)

(* Extra constructors (for convenience). *)

(* [arrows] builds a multi-argument function type. *)

val arrows: ftype list -> ftype -> ftype

(* [foralls] builds a multi-quantifier universal type. *)

val foralls: atom list -> ftype -> ftype

(* [wheres] builds a multi-equation constrained type scheme. *)

val wheres: ftype -> equations -> ftype

(* ------------------------------------------------------------------------- *)

(* Extra utilities. *)

(* [count_foralls ty] indicates how many universal quantifiers appear at the
   root of the type [ty]. This can be used, e.g., to report arity errors in
   a readable way. *)

val count_foralls: ftype -> int

