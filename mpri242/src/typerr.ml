open Printf
open Types
open Print

(* Auxiliary functions for the type-checker. *)

(* ------------------------------------------------------------------------- *)

(* Error messages. *)

let print_hypotheses xenv hyps =
  match hyps with
  | [] ->
      ""
  | _ ->
      sprintf "Hypotheses:\n%s" (print_equations xenv hyps)

let mismatch xenv loc hyps expected inferred =
  Error.error [loc] (sprintf
    "Type mismatch.\nExpected: %s\nInferred: %s\n%s"
    (print_type xenv expected)
    (print_type xenv inferred)
    (print_hypotheses xenv hyps)
  )

let unsatisfied_equation xenv loc hyps lhs rhs =
  Error.error [loc] (sprintf
    "This type equation should but does not hold:\n%s%s"
    (print_equations xenv [ lhs, rhs ])
    (print_hypotheses xenv hyps)
  )

let expected_form xenv loc form ty =
  Error.error [loc] (sprintf
    "Type mismatch: I expected %s type.\nInferred: %s\n"
    form
    (print_type xenv ty)
  )

let typecon_mismatch xenv loc d expected found =
  Error.error [loc] (sprintf
    "Data constructor mismatch.\n\
     Expected a data constructor associated with the type constructor: %s\n\
     Found the data constructor: %s\n\
     which is associated with the type constructor: %s\n"
    (print_atom xenv expected)
    (print_atom xenv d)
    (print_atom xenv found)
  )

let arity_mismatch xenv loc kind1 x kind2 expected found =
  Error.error [loc] (sprintf
    "The %s %s expects %d %s arguments,\nbut is applied to %d %s arguments.\n"
    kind1 (print_atom xenv x) expected kind2 found kind2
  )

let redundant_clause loc =
  Error.warning [loc] "Warning: this clause is redundant.\n"

let inaccessible_clause loc =
  Error.warning [loc] "Warning: this clause is inaccessible.\n"

let missing_clause xenv hyps loc dc =
  Error.error [loc] (sprintf
    "A case for the data constructor %s is missing.\n%s"
    (print_atom xenv dc)
    (print_hypotheses xenv hyps)
  )

(* ------------------------------------------------------------------------- *)

(* Functions that require a type to exhibit a certain shape, and deconstruct
   it. *)

let deconstruct_arrow xenv loc : ftype -> ftype * ftype =
  function
    | TyArrow (domain, codomain) ->
	domain, codomain
    | ty ->
	expected_form xenv loc "an arrow" ty

let deconstruct_univ xenv loc : ftype -> ftype_context =
  function
    | TyForall body ->
	body
    | ty ->
	expected_form xenv loc "a universal" ty

let deconstruct_tycon xenv loc : ftype -> Atom.atom =
  function
    | TyCon (tc, _) ->
	tc
    | ty ->
	expected_form xenv loc "an algebraic data" ty

