(* This module defines the abstract syntax of System F, as produced
   by [Internalize]. *)

(* all kinds of names are represented by unique atoms *)

open Error
open Atom
open Types

(* ------------------------------------------------------------------------- *)

(* Internal type information. *)

(* When the type-checker runs, it decorates the term with type information.
   This information is then used during the defunctionalization phase. *)

(* At functions, the type-checker records the following information: *)

type function_info = {

  (* The type equations that are assumed at this program point. *)

  hyps: equations;

  (* The type environment at this program point, just outside of the
     function. *)

  tenv: tenv;

  (* The type of the function. *)

  fty: ftype;

}

(* At function applications, the type-checker records the following information: *)

type application_info = {

  (* The type of the argument. *)

  domain: ftype;

  (* The type of the result. *)

  codomain: ftype;

}

(* ------------------------------------------------------------------------- *)

(* Terms. *)

type fterm =
  | TeVar of atom
      (* x *)
  | TeAbs of
      atom *                      (* function parameter *)
      ftype *                     (* function parameter's type *)
      fterm *                     (* function body *)
      function_info option ref    (* information recorded by the type-checker *)
  | TeApp of
      fterm *                     (* function *)
      fterm *                     (* argument *)
      application_info option ref (* information recorded by the type-checker *)
      (* t t *)
  | TeLet of atom * fterm * fterm
      (* let x = t in t *)
  | TeFix of atom * ftype * fterm
      (* fix x : T = t *)
  | TeTyAbs of atom * fterm
      (* fun [ a ] = t *)
  | TeTyApp of fterm * ftype
      (* t [ T ] *)
  | TeData of atom * ftype list * fterm list
      (* K [ T ... T ] { t; ...; t } *)
  | TeTyAnnot of fterm * ftype
      (* (t : T) *)
  | TeMatch of fterm * ftype * clause list
      (* match t return T with clause ... clause end *)
  | TeLoc of location * fterm
      (* t *)
      (* the parser generates [TeLoc] nodes to keep track of locations
	 within the source code. *)

and clause =
  | Clause of pattern * fterm
      (* p -> t *)

and pattern =
  | PatData of location * atom * atom list * atom list
      (* K [ a ... a ] { x; ...; x } *)

(* ------------------------------------------------------------------------- *)

(* The type constructor table maps a type constructor to its arity. *)

type type_table =
    int AtomMap.t

(* ------------------------------------------------------------------------- *)

(* The data constructor table maps a data constructor to its type scheme. *)

(* As explained in [Types], for the sake of uniformity, we view type schemes
   as types. However, type schemes are types of a certain form:

      forall a ... a. { T; ... ; T } -> tc a ... a where eq and ... and eq

   The tuple type constructor { T; ...; T } and the conjunction type constructor
   (T where T = T) are used only as part of type schemes, never as part of
   ordinary types. *)

type datacon_table =
    ftype AtomMap.t

(* ------------------------------------------------------------------------- *)

(* Programs. *)

type program =
  Prog of type_table * datacon_table * fterm

