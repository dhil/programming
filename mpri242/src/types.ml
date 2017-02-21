open Atom

(* ------------------------------------------------------------------------- *)

(* Types. *)

type ftype =
  | TyBoundVar of int
  | TyFreeVar of atom
  | TyArrow of ftype * ftype
  | TyForall of ftype_context
  | TyCon of atom * ftype list
  | TyTuple of ftype list
  | TyWhere of ftype * ftype * ftype

(* Internally, a type context is just a type in which the de Bruijn index 0 is
   allowed to occur. *)

(* A type context also records an identifier, whose sole purpose is to serve as
   a hint when pretty-printing. It can otherwise be ignored. *)

and ftype_context =
    Identifier.identifier * ftype

(* ------------------------------------------------------------------------- *)

(* Well-formedness. *)

let rec wf i = function
  | TyBoundVar j ->
      j < i
  | TyFreeVar _ ->
      true
  | TyArrow (domain, codomain) ->
      wf i domain && wf i codomain
  | TyForall (_, body) ->
      wf (i+1) body
  | TyCon (_, tys)
  | TyTuple tys ->
      List.for_all (wf i) tys
  | TyWhere (t, l, r) ->
      wf i t && wf i l && wf i r

(* ------------------------------------------------------------------------- *)

(* Equality. *)

let rec equal ty1 ty2 =
  match ty1, ty2 with
  | TyBoundVar i1, TyBoundVar i2 ->
      i1 = i2
  | TyFreeVar a1, TyFreeVar a2 ->
      Atom.equal a1 a2
  | TyArrow (domain1, codomain1), TyArrow (domain2, codomain2) ->
      equal domain1 domain2 && equal codomain1 codomain2
  | TyForall (_, body1), TyForall (_, body2) ->
      equal body1 body2
  | TyCon (tc1, tys1), TyCon (tc2, tys2) ->
      begin try
	Atom.equal tc1 tc2 && List.for_all2 equal tys1 tys2
      with Invalid_argument _ ->
	assert false (* arity error: should not happen *)
      end
  | TyTuple tys1, TyTuple tys2 ->
      begin try
	List.for_all2 equal tys1 tys2
      with Invalid_argument _ ->
	false (* arity mismatch: these are valid but distinct tuple types *)
      end
  |  TyWhere (t1, l1, r1), TyWhere (t2, l2, r2) ->
       equal t1 t2 && equal l1 l2 && equal r1 r2
  | _, _ ->
      false

(* ------------------------------------------------------------------------- *)

(* An occurs check. *)

exception Occurs

let occurs (p : atom -> bool) =

  let rec occurs = function
  | TyFreeVar a when p a ->
      raise Occurs
  | TyFreeVar _
  | TyBoundVar _ ->
      ()
  | TyArrow (domain, codomain) ->
      occurs domain;
      occurs codomain
  | TyForall (_, body) ->
      occurs body
  | TyCon (_, tys)
  | TyTuple tys ->
      List.iter occurs tys
  | TyWhere (t, l, r) ->
      occurs t;
      occurs l;
      occurs r

  in fun ty ->
    try
      occurs ty;
      false
    with Occurs ->
      true

(* ------------------------------------------------------------------------- *)

(* [ftv ty] returns the free type variables of the type [ty]. *)

let rec ftv accu = function
  | TyBoundVar _ ->
      accu
  | TyFreeVar a ->
      AtomSet.add a accu
  | TyArrow (ty1, ty2) ->
      let accu = ftv accu ty1 in
      let accu = ftv accu ty2 in
      accu
  | TyForall (_, body) ->
      ftv accu body
  | TyCon (_, tys)
  | TyTuple tys ->
      List.fold_left ftv accu tys
  | TyWhere (t, l, r) ->
      let accu = ftv accu t in
      let accu = ftv accu l in
      let accu = ftv accu r in
      accu

let ftv ty =
  ftv AtomSet.empty ty

(* ------------------------------------------------------------------------- *)

(* Context creation. *)

let rec abstract a i ty =
  match ty with
  | TyFreeVar b when Atom.equal a b ->
      TyBoundVar i
  | TyFreeVar _
  | TyBoundVar _ ->
      ty
  | TyArrow (domain, codomain) ->
      TyArrow (abstract a i domain, abstract a i codomain)
  | TyForall (hint, body) ->
      TyForall (hint, abstract a (i+1) body)
  | TyCon (tc, tys) ->
      TyCon (tc, List.map (abstract a i) tys)
  | TyTuple tys ->
      TyTuple (List.map (abstract a i) tys)
  | TyWhere (t, l, r) ->
      TyWhere (abstract a i t, abstract a i l, abstract a i r)

let abstract a ty =
  let hint = Atom.identifier a in
  hint, abstract a 0 ty

(* ------------------------------------------------------------------------- *)

(* Context elimination. *)

let rec fill i ty c =
  match c with
  | TyBoundVar j when i = j ->
      ty
  | TyBoundVar _
  | TyFreeVar _ ->
      c
  | TyArrow (domain, codomain) ->
      TyArrow (fill i ty domain, fill i ty codomain)
  | TyForall (hint, body) ->
      TyForall (hint, fill (i+1) ty body)
  | TyCon (tc, tys) ->
      TyCon (tc, List.map (fill i ty) tys)
  | TyTuple tys ->
      TyTuple (List.map (fill i ty) tys)
  | TyWhere (t, l, r) ->
      TyWhere (fill i ty t, fill i ty l, fill i ty r)

let fill (_, c) ty =
  fill 0 ty c

(* ------------------------------------------------------------------------- *)

(* Hints. *)

let hint (hint, _) =
  hint

(* ------------------------------------------------------------------------- *)

(* Typing environments map term variables to types. *)

type tenv =
    ftype AtomMap.t

let empty : tenv =
  AtomMap.empty

let lookup : atom -> tenv -> ftype =
  fun x tenv ->
    try
      AtomMap.find x tenv
    with Not_found ->
      assert false (* should not happen, if there is a binding of [x] in [tenv] *)

let bind : atom -> ftype -> tenv -> tenv =
  AtomMap.add

let binds xts tenv =
  List.fold_left (fun tenv (x, ty) ->
    bind x ty tenv
  ) tenv xts

(* ------------------------------------------------------------------------- *)

(* A conjunction of type equations is represented as a list. *)

type equations =
    (ftype * ftype) list

(* ------------------------------------------------------------------------- *)

(* Extra constructors. *)

let arrows arguments result =
  List.fold_right (fun argument result ->
    TyArrow (argument, result)
  ) arguments result

let foralls quantifiers result =
  List.fold_right (fun quantifier result ->
    TyForall (abstract quantifier result)
  ) quantifiers result

let wheres ty equations =
  List.fold_left (fun t (l, r) -> TyWhere (t, l, r)) ty equations

(* ------------------------------------------------------------------------- *)

(* Extra utilities. *)

let rec count_foralls accu = function
  | TyForall (_, body) ->
      count_foralls (accu+1) body
  | _ ->
      accu

let count_foralls ty =
  count_foralls 0 ty

