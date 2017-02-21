open Atom
open Types
open Terms

(* ------------------------------------------------------------------------- *)

(* [fv term] is the set of the free term variables of [term]. *)

let sremove (sa : atom list) (s : AtomSet.t) : AtomSet.t =
  List.fold_right AtomSet.remove sa s

let union_map (f : 'a -> AtomSet.t) (xs : 'a list) : AtomSet.t =
  List.fold_left (fun accu x ->
    AtomSet.union accu (f x)
  ) AtomSet.empty xs

let rec fv = function
  | TeVar x ->
      AtomSet.singleton x
  | TeAbs (x, _, body, _)
  | TeFix (x, _, body) ->
      AtomSet.remove x (fv body)
  | TeApp (term1, term2, _) ->
      AtomSet.union (fv term1) (fv term2)
  | TeLet (x, term1, term2) ->
      AtomSet.union (fv term1) (AtomSet.remove x (fv term2))
  | TeTyAbs (_, term)
  | TeTyApp (term, _)
  | TeTyAnnot (term, _)
  | TeLoc (_, term) ->
      fv term
  | TeData (_, _, fields) ->
      union_map fv fields
  | TeMatch (term, _, clauses) ->
      AtomSet.union (fv term) (union_map fv_clause clauses)

and fv_clause = function
  | Clause (PatData (_, _, _, tevars), term) ->
      sremove tevars (fv term)

(* ------------------------------------------------------------------------- *)

(* [head] extracts the type constructor that lies at the head of a type
   scheme. *)

let rec head = function
  | TyBoundVar _
  | TyFreeVar _
  | TyTuple _ ->
      assert false
  | TyArrow (_, ty)
  | TyWhere (ty, _, _) ->
      head ty
  | TyForall body ->
      let dummy = TyBoundVar 0 in
      head (fill body dummy)
  | TyCon (tc, _) ->
      tc

let type_scheme (Prog (_, dctable, _)) dc =
  AtomMap.find dc dctable

let type_constructor p dc =

  (* Find the type scheme associated with [dc], and go down into it
     to extract its head. This is inefficient, but good enough for us,
     and avoids the need to build a redundant table. *)

  head (type_scheme p dc)

let data_constructors (Prog (_, dctable, _)) tc =

  (* Iterate over all data constructors, and find those that are associated
     with [tc]. This is inefficient, but good enough for us, and avoids the
     need to build a redundant table. *)

  AtomMap.fold (fun dc s dcs ->
    if Atom.equal tc (head s) then
      AtomSet.add dc dcs
    else
      dcs
  ) dctable AtomSet.empty

