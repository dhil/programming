open Atom
open Syntax
open Types
open Terms

type identifier =
    Identifier.identifier

type atom =
    Atom.atom

type env =
    Import.env

(* ------------------------------------------------------------------------- *)

(* Utilities. *)

let omap f = function
  | None ->
      None
  | Some x ->
      Some (f x)

(* ------------------------------------------------------------------------- *)

(* Term variables, type variables, type constructors, and data constructors are
   explicitly bound. *)

let bind env id : atom * env =
  let env = Import.bind env id in
  Import.resolve env id, env

(* ------------------------------------------------------------------------- *)

(* [mktc] builds a [TyCon] type and checks that its arity is respected. *)

let mktc tctable env id tys =
  let tc = Import.resolve env id in
  let got = List.length tys in
  let expected =
    try
      AtomMap.find tc tctable
    with Not_found ->
      (* if this fails, then [tc] is unbound and [resolve] has failed silently;
	 in that case, do not report an arity error *)
      got
  in
  if expected <> got then
    Error.signal
      [ Identifier.location id ]
      (Printf.sprintf "The type constructor %s expects %d arguments, but is here applied to %d arguments.\n"
	(Identifier.name id) expected got);
  TyCon (tc, tys)

(* ------------------------------------------------------------------------- *)

(* [itype] converts a type from external syntax into internal syntax. *)

let ityvar env a =
  TyFreeVar (Import.resolve env a)

let ityvars env =
  List.map (ityvar env)

let rec itype tctable env : Syntax.ftype -> Types.ftype = function
  | SynTyArrow (domain, codomain) ->
      TyArrow (itype tctable env domain, itype tctable env codomain)
  | SynTyForall (a, body) ->
      let a, env = bind env a in
      TyForall (abstract a (itype tctable env body))
  | SynTyVarOrTyCon (id, []) when Import.resolves env type_sort id ->
      (* if there are no type arguments and if [id] resolves as a type variable [a],
         then consider it is a type variable *)
      let a = Identifier.mak type_sort id in
      ityvar env a
  | SynTyVarOrTyCon (id, tys) ->
      (* otherwise, consider [id] must be a type constructor *)
      let tc = Identifier.mak typecon_sort id in
      mktc tctable env tc (itypes tctable env tys)

and itypes tctable env tys =
  List.map (itype tctable env) tys

(* ------------------------------------------------------------------------- *)

(* [ischeme] converts a data constructor type scheme in external syntax into
   a type in internal syntax. *)

let ischeme tctable env : Syntax.scheme -> Types.ftype = function
  | SynScheme (quantifiers, eqs, fields, tc, params) ->

      (* Deal with the universal quantifiers. *)

      let env = Import.bind_simultaneously env quantifiers in
      List.fold_right (fun quantifier ty -> 
	let quantifier = Import.resolve env quantifier in
	TyForall (abstract quantifier ty)
      ) quantifiers (

	(* Deal with the equations. *)

	List.fold_right (fun (SynEq (l, r)) ty ->
	  TyWhere (ty, itype tctable env l, itype tctable env r)
	) eqs (

	  (* Build a function type whose domain is a tuple of the fields and
	     whose codomain is an application of the type constructor [tc] to
	     the type variables [params]. *)
      
	  TyArrow (
	    TyTuple (itypes tctable env fields),
	    mktc tctable env tc (itypes tctable env params)
	  )
	)
      )

(* ------------------------------------------------------------------------- *)

(* [iterm] converts a term from external syntax into internal syntax. *)

let rec iterm tctable env = function

  | SynTeVar id ->
      TeVar (Import.resolve env id)

  | SynTeAbs (x, ftype, term) ->
      let x, env = bind env x in
      TeAbs (x, itype tctable env ftype, iterm tctable env term, ref None)

  | SynTeApp (term1, term2) ->
      TeApp (iterm tctable env term1, iterm tctable env term2, ref None)

  | SynTeLet (x, term1, term2) ->
      let x, env' = bind env x in
      TeLet (x, iterm tctable env term1, iterm tctable env' term2)

  | SynTeFix (x, ftype, term) ->
      let x, env = bind env x in
      TeFix (x, itype tctable env ftype, iterm tctable env term)

  | SynTeTyAbs (a, term) ->
      let a, env = bind env a in
      TeTyAbs (a, iterm tctable env term)

  | SynTeTyApp (term, ftype) ->
      TeTyApp (iterm tctable env term, itype tctable env ftype)

  | SynTeData (id, tys, fields) ->
      TeData (Import.resolve env id, itypes tctable env tys, iterms tctable env fields)

  | SynTeTyAnnot (t, ty) ->
      TeTyAnnot (iterm tctable env t, itype tctable env ty)

  | SynTeMatch (t, ty, clauses) ->
      TeMatch (iterm tctable env t, itype tctable env ty, iclauses tctable env clauses)

  | SynTeLoc (loc, t) ->
      TeLoc (loc, iterm tctable env t)

and iterms tctable env terms =
  List.map (iterm tctable env) terms

and iclauses tctable env clauses =
  List.map (iclause tctable env) clauses

and iclause tctable env = function
  | SynClause (p, t) ->
      let p, env = ipattern env p in
      Clause (p, iterm tctable env t)

and ipattern env = function
  | SynPatData (loc, d, tyvars, fields) ->
      let env, tyvars = Import.bind_sequentially env tyvars in
      let env, fields = Import.bind_sequentially env fields in
      PatData (loc, Import.resolve env d, tyvars, fields), env

(* ------------------------------------------------------------------------- *)

(* [build] builds the initial toplevel environment, which binds all
   type constructors and all data constructors. It also builds the
   type constructor and data constructor tables. *)

let build ds : type_table * datacon_table * env =

  let env = Import.empty in

  (* Gather all type constructors. *)

  let tcs : identifier list =
    List.fold_left (fun tcs -> function SynType (tc, _) -> tc :: tcs | _ -> tcs) [] ds
  in

  (* Build an import environment for type constructors. *)

  let env = Import.bind_simultaneously env tcs in
  Error.signaled();

  (* Build a table that maps a type constructor to its arity. *)

  let tctable : int AtomMap.t =
    List.fold_left (fun tctable -> function
      | SynType (tc, params) -> AtomMap.add (Import.resolve env tc) (List.length params) tctable
      | _ -> tctable
    ) AtomMap.empty ds
  in

  (* Gather all data constructors. *)

  let dcs : identifier list =
    List.fold_left (fun dcs -> function SynDatacon (d, _) -> d :: dcs | _ -> dcs) [] ds
  in

  (* Extend the import environment with the data constructors. *)

  let env = Import.bind_simultaneously env dcs in

  (* Build a table that maps a data constructor to its type scheme. *)

  let dctable : ftype AtomMap.t =
    List.fold_left (fun dctable -> function
      | SynDatacon (d, s) -> AtomMap.add (Import.resolve env d) (ischeme tctable env s) dctable
      | _ -> dctable
    ) AtomMap.empty ds
  in

  tctable, dctable, env

(* ------------------------------------------------------------------------- *)

(* [program] converts a complete program. *)

let program = function
  | SynProg (ds, t) ->
      let tctable, dctable, env = build ds in
      let t = iterm tctable env t in
      Error.signaled();
      Prog (tctable, dctable, t)

