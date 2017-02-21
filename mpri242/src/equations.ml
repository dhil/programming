open Atom
open Types

(* Unification. *)

(* Unification of System F types resembles first-order unification under a
   mixed prefix: that is, it requires keeping track of which unification
   variables are existentially quantified, or flexible, and which are
   universally quantified, or rigid. In fact, the problem appears
   slightly more complex than first-order unification under a mixed
   prefix, because System F types are not first-order terms: they
   contain binders, due to the [TyForall] construct, and the unifier
   must be able to deal with these binders by instantiating them with
   fresh variables. *)

(* ------------------------------------------------------------------------- *)

(* Technical remarks. *)

(* We work with arbitrarily large terms, as opposed to terms of depth 1.
   This is perhaps less efficient -- an efficient first-order unification
   algorithm works with terms of depth 1, as this enables more sharing --
   but seems easier to understand when terms contain binders, as is the
   case here, due to [TyForall]. *)

(* Thus, the structure that can be carried by a variable is just a type,
   as defined in module [Types]. *)

(* The decision to work with arbitrarily large terms implies that the
   occurs check must be performed eagerly. (If we allowed cyclic
   structures to appear, we would not be able to detect that we are
   looping. For instance, if we allowed the cyclic structures a = FFa
   and b = FFb, then examining the goal a = Fb would cause us to loop
   without performing any further unifications.) An eager occurs check
   is inefficient -- an efficient first-order unification algorithm
   allows cyclic structures to appear, and delays the occurs check --
   but it seems that this cannot be helped. *)

(* Types may contain free atoms. We use a unification environment
   [uenv] to map atoms to the corresponding unification variables. *)

(* We work with unification problems of the form exists/forall. That
   is, we distinguish between flexible variables and rigid variables.
   The former are existentially quantified, while the latter are
   universally quantified. Furthermore, any attempt to unify a
   flexible variable with a term that contains a rigid free variable
   causes a failure. *)

(* ------------------------------------------------------------------------- *)

(* A unification variable is implemented with the help of the [UnionFind]
   algorithm. It carries a descriptor that contains its structure -- a type --
   and a flag that tells whether the variable is flexible or rigid. Both
   pieces of information are mutable. The [structure] field is written when a
   variable is unified with a type. The [flexible] field is written by
   [rigidify]. *)

type variable =
    descriptor UnionFind.point

and descriptor = {
  mutable structure: ftype;
  mutable flexible: bool
}

(* ------------------------------------------------------------------------- *)

(* A unification environment maps the free atoms of the unification problem
   to the unification variables that stand for them. *)

(* We make the unification environment modifiable, using a reference at its
   root, so it can be extended during unification, without requiring the
   unifier to return a new unification environment as an extra result. *)

type uenv =
    variable AtomMap.t ref

(* The function [atom_to_variable uenv a] consults this mapping. *)

let atom_to_variable (uenv : uenv) (a : atom) : variable =
  try
    AtomMap.find a !uenv
  with Not_found ->
    assert false

(* The [register] functions create fresh flexible unification variables for
   the free atoms of their arguments. They are used to build an initial
   unification environment. *)

let register_atom uenv a : unit =
  if not (AtomMap.mem a !uenv) then
    let v = UnionFind.fresh {
      structure = TyFreeVar a;
      flexible = true
    } in
    uenv := AtomMap.add a v !uenv

let register_ty uenv ty =
  AtomSet.iter (register_atom uenv) (ftv ty)

let register_equation uenv (ty1, ty2) =
  register_ty uenv ty1;
  register_ty uenv ty2

let register_equations uenv (eqs : equations) =
  List.iter (register_equation uenv) eqs

(* [rigidify uenv] makes all unification variables rigid. Once this is
   done, no actual unification can be performed: all calls to [unify]
   amount to equality checks. *)

let rigidify uenv =
  AtomMap.iter (fun _ v ->
    let desc = UnionFind.find v in
    desc.flexible <- false
  ) !uenv

(* ------------------------------------------------------------------------- *)

(* Types contain free atoms, but atoms are to be regarded as proxies for
   unification variables, which in turn carry some structure. So, a type
   of the form [TyFreeVar a] might be just an abbreviation for some other
   type. The function [normalize] expands away this abbreviation. *)

(* [normalize] is idempotent. If it returns a type of the form [TyFreeVar b],
   then [b] really represents a type variable -- that is, it is not a proxy
   for some other type. This property is checked by an assertion below, which
   makes use of the auxiliary function [final]. The function [final] is
   otherwise useless. *)

let final uenv ty =
  match ty with
  | TyFreeVar a ->
      let v = atom_to_variable uenv a in
      let desc = UnionFind.find v in
      begin match desc.structure with TyFreeVar b -> Atom.equal a b | _ -> false end
  | _ ->
      true

let normalize uenv (ty : ftype) : ftype =
  match ty with
  | TyFreeVar a ->
      let v = atom_to_variable uenv a in
      let desc = UnionFind.find v in
      assert (final uenv desc.structure); (* this assertion could be removed *)
      desc.structure
  | _ ->
      ty

(* ------------------------------------------------------------------------- *)

(* The occurs check must be modified to perform on-the-fly normalization at
   free atoms. I almost forgot. *)

let rec occurs uenv p ty =
  Types.occurs (fun a ->
    match normalize uenv (TyFreeVar a) with
    | TyFreeVar a ->
	p a
    | ty ->
	occurs uenv p ty (* that is a cool recursive call *)
  ) ty

(* ------------------------------------------------------------------------- *)

(* The unifier. *)

exception Unify

let rec unify uenv (ty1 : ftype) (ty2 : ftype) : unit =
  let ty1 = normalize uenv ty1
  and ty2 = normalize uenv ty2 in
  match ty1, ty2 with

  (* We have two variables. Merge their equivalent classes, if this has
     not been done already. Keep an arbitrary descriptor among the two. *)

  (* Unification is permitted only if the variables are both flexible.
     Unifying a flexible variable and a rigid variable is forbidden
     because we are working with an exists/forall prefix: the rigid
     variables are more recent than the flexible variables. Unifying
     two rigid variables is forbidden if they are distinct. *)

  | TyFreeVar a1, TyFreeVar a2 ->
      let v1 = atom_to_variable uenv a1
      and v2 = atom_to_variable uenv a2 in
      let desc1 = UnionFind.find v1
      and desc2 = UnionFind.find v2 in
      if not (UnionFind.equivalent v1 v2) then begin
	assert (not (Atom.equal a1 a2)); (* a single atom should not be carried by two distinct unification variables *)
	if desc1.flexible && desc2.flexible then
	  UnionFind.union v1 v2
	else
	  raise Unify
      end

  (* We have a variable [v1] on one side, and structure [ty2] on the other side.
     then perform unification, by updating [v1]'s descriptor. *)

  (* Unification is permitted only if [v1] does not occur within [ty2] -- this is
     the usual occurs check. *)

  (* Unification is permitted only if [v1] is flexible (obviously) and only if
     [ty2] contains no rigid variables (again, this is because we are working with
     an exists/forall prefix). *)

  | TyFreeVar a1, ty2
  | ty2, TyFreeVar a1 ->
      let v1 = atom_to_variable uenv a1 in
      let desc1 = UnionFind.find v1 in
      if desc1.flexible && 
	 not (occurs uenv (fun a2 ->
	   let v2 = atom_to_variable uenv a2 in
	   let desc2 = UnionFind.find v2 in
	   UnionFind.equivalent v1 v2 || not desc2.flexible
	 ) ty2)
      then
	desc1.structure <- ty2
      else
	raise Unify

  (* We have structure on both sides. Decompose the equation. *)

  | TyBoundVar i1, TyBoundVar i2 when i1 = i2 ->
      ()

  | TyArrow (domain1, codomain1), TyArrow (domain2, codomain2) ->
      unify uenv domain1 domain2;
      unify uenv codomain1 codomain2

  | TyCon (tc1, args1), TyCon (tc2, args2) when Atom.equal tc1 tc2 ->
      begin try
	  List.iter2 (unify uenv) args1 args2
	with Invalid_argument _ ->
	  assert false (* should not happen *)
      end

  (* We have a universal quantifier on both sides. Decomposing the equation
     requires introducing a fresh atom [a] together with a fresh rigid
     unification variable [v]. The descriptor for [v] refers to [a],
     while conversely, the unification environment is extended with a
     mapping of [a] to [v]. The bodies of the two universally quantified
     types are instantiated with [a], and unification continues. *)
	
  | TyForall body1, TyForall body2 ->
      let a = Atom.fresh (hint body1) in
      let ta = TyFreeVar a in
      let v = UnionFind.fresh {
	structure = ta;
	flexible = false
      } in
      uenv := AtomMap.add a v !uenv;
      unify uenv (fill body1 ta) (fill body2 ta)

  (* Some constructs are supposed to appear in type schemes, not in types,
     so we do not deal with them here. *)

  | TyTuple _, _
  | _, TyTuple _
  | TyWhere _, _
  | _, TyWhere _ ->
      assert false (* should not happen *)

  (* Mismatch. The unification problem is unsatisfiable. *)

  | _, _ ->
      raise Unify

let unify_equation uenv (ty1, ty2) =
  unify uenv ty1 ty2

let unify_equations uenv (eqs : equations) =
  List.iter (unify_equation uenv) eqs

(* ------------------------------------------------------------------------- *)

(* These wrappers catch the exception [Unify] and return a Boolean result. *)

let success f x =
  try
    f x;
    true
  with Unify ->
    false

let failure f x =
  not (success f x)

(* ------------------------------------------------------------------------- *)

(* [entailment hyps goals] tells whether the hypotheses [hyps] entail
   the equations [goals]. *)

let entailment (hyps : equations) (goals : equations) =

  (* Build up an initial unification environment where each free atom of the
     problem is associated with a flexible unification variable. *)

  let uenv : uenv = ref AtomMap.empty in
  register_equations uenv hyps;
  register_equations uenv goals;

  (* Compute the most general unifier of the hypotheses. Then, check that this
     unifier is a solution of the goal. During the latter check, no unification
     should take place, so [rigidify] is used prior to this check. *)

  (* Recall that a logical implication [A implies B] can be written [not A || B].
     We use this encoding, and exploit the short-circuit character of the disjunction
     operator. That is, if the unification of the hypotheses fails, then [entailment]
     returns [true] without evaluating the goal. *)

  failure (unify_equations uenv) hyps ||
  (rigidify uenv; success (unify_equations uenv) goals)

(* ------------------------------------------------------------------------- *)

(* [inconsistent hyps] tells whether the hypotheses [hyps] are
   inconsistent, that is, whether they imply the goal [false]. *)

let inconsistent (hyps: equations) =

  (* Build up an initial unification environment where each free atom of the
     problem is associated with a flexible unification variable. *)

  let uenv : uenv = ref AtomMap.empty in
  register_equations uenv hyps;

  (* Compute the most general unifier of the hypotheses. If this fails,
     then the hypotheses are inconsistent. *)

  failure (unify_equations uenv) hyps

