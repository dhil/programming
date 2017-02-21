(* This module defines a pretty-printer for System F types and terms
   in internal form. It relies on the basic machinery provided by
   [Export]. *)

module B = Buffer
open Atom
open Types
open Pprint
open Terms
open Symbols

(* ------------------------------------------------------------------------- *)

(* Utilities. *)

let omap f = function
  | None ->
      None
  | Some x ->
      Some (f x)

(* ------------------------------------------------------------------------- *)

(* Printing atoms. *)

let pvar env x =
  text (Identifier.name (Export.resolve env x))

(* ------------------------------------------------------------------------- *)

(* Syntactic elements for the pretty-printer. *)

let forall =
  text "forall"

let arrow =
  text " ->"

let lambda =
  text "fun"

let colon =
  text " :" ^^ softline

let equal =
  text " ="

let where =
  text "where "

let kmatch =
  text "match "

let return =
  softline ^^ text "return" ^^ softline

let kwith =
  text " with" ^^ line

let kend =
  text "end"

let bar =
  text "| "

let fix =
  text "fix"

(* ------------------------------------------------------------------------- *)

(* Syntactic idioms for the pretty-printer. *)

(* [heading head body] prints [head]; breaks a line and indents by 2,
   if necessary; then prints [body]. *)

let heading head body =
  group (
    nest 2 (
      group head ^^ linebreak ^^
      body
    )
  )

(* [jump body] either displays a space, followed with [body], followed
   with a space, all on a single line; or breaks a line, prints [body]
   at indentation 2, and breaks another line. *)

let jump body =
  group (nest 2 (line ^^ body) ^^ line)

(* [definition head body cont] prints [head]; prints [body], surrounded
   with spaces and, if necessary, indented by 2; prints the keyword [in];
   breaks a line, if necessary; and prints [cont]. *)

let definition head body cont =
  group (
    group head ^^ jump body ^^ text "in"
  ) ^^ line ^^
  cont

(* ------------------------------------------------------------------------- *)

(* Tuples. *)

let pfields f fields =
  match fields with
  | [] ->
      braces empty
  | _ ->
      braces (
	jump (
	  sepmap
	  (semi ^^ softline) (* [softline] allows several fields per line, with wrapping *)
	  (fun field -> group (f field))
	  fields
	)
      )

(* ------------------------------------------------------------------------- *)

(* Applications. *)

let app f2 t1 t2 =
  t1 ^^ line ^^ f2 t2

let apps f2 t1 t2s =
  List.fold_left (app f2) t1 t2s

(* ------------------------------------------------------------------------- *)

(* Types. *)

(* In order to provide proper parenthesization, the structure of the
   printing functions reflects that of the parser: there are several
   levels of priority. *)

let rec pty0 env ty =
  match ty with
  | TyBoundVar _ ->
      assert false
  | TyFreeVar a ->
      pvar env a
  | TyCon (tc, []) ->
      pvar env tc
  | TyTuple fields ->
      pfields (pty env) fields
  | _ ->
      parens (pty env ty)

and pty1 env ty =
  match ty with
  | TyCon (tc, tys) ->
      apps (pty0 env) (pvar env tc) tys
  | _ ->
      pty0 env ty

and pty env ty =
  group (
    match ty with
    | TyArrow (domain, codomain) ->
	pty1 env domain ^^
	arrow ^^ softline ^^
	pty env codomain
    | TyForall _ ->
	pforall env [] ty
    | TyWhere (t, l, r) ->
	(* there could be ambiguity if [TyWhere] was part of the syntax of ordinary
	   types; here, things should be OK because we use [TyWhere] only as part
	   of the syntax of data constructor definitions *)
	(* we indent the first [where] clause by 2 and the following [where] clauses
	   by 0, so that all [where] clauses are aligned *)
	nest
	  (match t with TyWhere _ -> 0 | _ -> 2)
	  (pty env t ^^ line ^^ where ^^ pty env l ^^ equal ^^ softline ^^ pty env r)
    | _ ->
	pty1 env ty
  )

(* Group multiple consecutive universal quantifiers. *)

and pforall env qs = function
  | TyForall body ->
      let a = Atom.fresh (Types.hint body) in
      let env = Export.bind env a in
      pforall env (a :: qs) (fill body (TyFreeVar a))
  | ty ->
      nest 2 (
	forall ^^
	catmap (fun a -> space ^^ pvar env a) (List.rev qs) ^^ dot ^^ line ^^
	pty env ty
      )

(* ------------------------------------------------------------------------- *)

(* Equations. *)

let peq env (ty1, ty2) =
  group (pty env ty1 ^^ equal ^^ softline ^^ pty env ty2) ^^ linebreak

let peqs env eqs =
  catmap (peq env) eqs

(* ------------------------------------------------------------------------- *)

(* Types within brackets. *)

and brackets_pty env ty =
  brackets (pty env ty)

let brackets_pvar env a =
  brackets (pvar env a)

(* ------------------------------------------------------------------------- *)

(* The pretty-printer for terms (below) is quite fancy because it reconstructs
   some of the syntactic sugar that the parser eliminates. This is done by
   carrying a context top-down. 

   Eliminating syntactic sugar sometimes involves eliminating redundant
   information. If this information is in fact not redundant but inconsistent,
   elimination of the syntactic sugar is impossible and fails. We catch this
   failure and revert to the normal mode of printing one constructor at a time. *)

(* The following type definition indicates what contextual information
   is carried down. *)

(* TEMPORARY one could be even more ambitious and include a [let (rec)] at top
   of context; would require an export pun *)

type def_context =
  | DC of
      atom option *           (* fix f in [] *)
      atom list *             (* fun [ a ... a ] = [] *)
      (atom * ftype) list *   (* fun (x : T) ... (x : T) = [] *)
      ftype option            (* ([] : T) *)

let empty_dc =
  DC (None, [], [], None)

exception Inconsistency

(* ------------------------------------------------------------------------- *)

(* Terms. Normal mode. *)

let rec pterm0 env = function
  | TeLoc (_, term) ->
      pterm0 env term
  | TeVar x ->
      pvar env x
  | TeData (dc, tys, fields) ->
      app (pfields (pterm env)) (apps (brackets_pty env) (pvar env dc) tys) fields
  | TeTyAnnot (term, ty) ->
      parens (pterm env term ^^ colon ^^ pty env ty)
  | TeMatch (term, ty, clauses) ->
      kmatch ^^ pterm env term ^^ return ^^ pty env ty ^^ kwith ^^
      catmap (pclause env) clauses ^^
      kend
  | term ->
      parens (pterm env term)

and pterm1 env term =
  group (match term with
  | TeLoc (_, term) ->
      pterm1 env term
  | TeApp (term1, term2, _) ->
      pterm1 env term1 ^^ line ^^ pterm0 env term2
  | TeTyApp (term, ty) ->
      pterm1 env term ^^ line ^^ brackets_pty env ty
  | _ ->
      pterm0 env term
  )

and pterm env term =
  group (match term with
  | TeLoc (_, term) ->
      pterm env term
  | TeAbs (x, domain, body, _) ->
      begin try
	(* move to context-carrying mode, with an empty context *)
	pterm_with_dc env empty_dc term
      with Inconsistency ->
	(* if this fails, revert to simple mode *)
	pdef env None [] [ x, domain ] None body
      end
  | TeTyAbs (a, body) ->
      begin try
	(* move to context-carrying mode, with an empty context *)
	pterm_with_dc env empty_dc term
      with Inconsistency ->
	(* if this fails, revert to simple mode *)
        pdef env None [ a ] [] None body
      end
  | TeFix (x, ty, body) ->
      begin try
	(* move to context-carrying mode, with an empty context *)
	pterm_with_dc env empty_dc term
      with Inconsistency ->
	(* if this fails, revert to simple mode *)
	let env = Export.bind env x in
	heading
	  (fix ^^ space ^^ pvar env x ^^ colon ^^ pty env ty ^^ text " in")
	  (pterm env body)
      end
  | TeLet (x, term1, term2) ->
      let term1 = pterm env term1 in
      let env = Export.bind env x in
      definition
	(text "let" ^^ line ^^ pvar env x ^^ equal)
	term1
	(pterm env term2)
  | _ ->
      pterm1 env term
  )

(* ------------------------------------------------------------------------- *)

(* Terms. Context-carrying mode. *)

and pterm_with_dc env dc term =
  match dc, term with
  | _, TeLoc (_, term) ->
      pterm_with_dc env dc term
  | DC (f, ty_args, te_args, oty), TeTyAnnot (body, ty) ->
      (* accumulate one type annotation and continue in context-carrying mode *)
      pterm_with_dc
	env
	(DC (f, ty_args, te_args, confront_oty oty ty))
	body
  | DC (f, ty_args, te_args, oty), TeAbs (x, domain, body, _) ->
      (* accumulate one term argument and continue in context-carrying mode *)
      pterm_with_dc
	env
	(DC (f, ty_args, te_args @ [ x, domain ], apply_oty oty domain))
	body
  | DC (f, ty_args, [], oty), TeTyAbs (a, body) ->
      (* accumulate one type argument and continue in context-carrying mode *)
      pterm_with_dc
	env
	(DC (f, ty_args @ [ a ], [], instantiate_oty oty a))
	body
  | DC (None, [], [], None), TeFix (f, ty, body) ->
      (* accumulate a fix and continue in context-carrying mode *)
      pterm_with_dc
	env
	(DC (Some f, [], [], Some ty))
	body
  | DC (f, ty_args, te_args, oty), _ ->
      (* the form of contexts does not allow accumulating any more; *)
      (* so print what we have accumulated so far and continue in
	 normal mode. *)
      pdef env f ty_args te_args oty term

and confront_oty oty1 ty2 =
  (* Presumably the type annotation [oty1], if present, is equal to
     [ty2]. Otherwise, the term that we are trying to print is ill-typed! *)
  match oty1 with
  | None ->
      Some ty2
  | Some ty1 when Types.equal ty1 ty2 ->
      Some ty2
  | Some _ ->
      raise Inconsistency

and apply_oty oty domain1 =
  (* Presumably the type annotation [oty], if present, is a function
     type, and its domain is [domain1]. Otherwise, the term that we
     are trying to print is ill-typed! *)
  match oty with
  | None ->
      None
  | Some (TyArrow (domain2, codomain)) when Types.equal domain1 domain2 ->
      Some codomain
  | Some _ ->
      raise Inconsistency

and instantiate_oty oty a =
  (* Presumably the type annotation [oty], if present, is a universal
     type. Otherwise, the term that we are trying to print is ill-typed! *)
  match oty with
  | None ->
      None
  | Some (TyForall body) ->
      Some (fill body (TyFreeVar a))
  | Some _ ->
      raise Inconsistency

(* ------------------------------------------------------------------------- *)

(* Terms. Definitions. *)

and pdef env f tyvars tevars ocodomain body =
  let env = Export.obind env f in
  let env = Export.sbind env tyvars in
  let env = Export.sbind env (List.map fst tevars) in
  heading
    (
      (* exploit the fact that [fix] can always be replaced by [fun] *)
      lambda ^^ 
      optional (fun f -> space ^^ pvar env f) f ^^
      catmap (ptype_argument env) tyvars ^^
      catmap (pterm_argument env) tevars ^^
      optional (fun ty -> colon ^^ pty env ty) ocodomain ^^
      equal ^^ space
    )
    (pterm env body)

and ptype_argument env tyvar =
  space ^^ brackets_pvar env tyvar

and pterm_argument env (x, ty) =
  space ^^ parens (pvar env x ^^ colon ^^ pty env ty)

(* ------------------------------------------------------------------------- *)

(* Clauses. *)

and pclause env = function
  | Clause (PatData (_, dc, tyvars, tevars), term) ->
      let env = Export.sbind env tyvars in
      let env = Export.sbind env tevars in
      nest 4 (
	group (
	  bar ^^
	  app (pfields (pvar env)) (apps (brackets_pvar env) (pvar env dc) tyvars) tevars ^^
	  arrow
	) ^^
	line ^^
	pterm env term
      ) ^^ line

(* ------------------------------------------------------------------------- *)

(* Algebraic data types. *)

let rec print_arity arity =
  if arity = 0 then empty else text " _" ^^ print_arity (arity - 1)

let print_dc p env dc =
  heading
    (text "data constructor " ^^ pvar env dc ^^ colon)
    (pty env (type_scheme p dc))

let print_tc p env tc arity =

  (* Print the type constructor declaration. *)

  text "type " ^^ pvar env tc ^^ print_arity arity ^^ linebreak ^^

  (* Print the associated data constructors. *)

  AtomSet.fold (fun dc accu ->
    print_dc p env dc ^^ linebreak ^^ accu
  ) (data_constructors p tc) empty

(* ------------------------------------------------------------------------- *)

(* Programs. *)

let print_program (Prog (tctable, dctable, term) as p) =

  (* Build an export environment. *)

  let env = Export.empty in
  let env = AtomMap.fold (fun tc _ env -> Export.bind env tc) tctable env in
  let env = AtomMap.fold (fun dc _ env -> Export.bind env dc) dctable env in

  (* Print the algebraic data type definitions. *)

  AtomMap.fold (fun tc arity accu ->
    print_tc p env tc arity ^^ accu
  ) tctable empty ^^

  (* Print the main program. *)

  text "program" ^^ line ^^
  pterm env term

(* ------------------------------------------------------------------------- *)

(* Wrapping up. *)

let b =
  B.create 2048

let doc2string doc =
  B.clear b;
  Pprint.Buffer.pretty 0.95 78 b doc;
  B.contents b

let print_atom env ty =
  doc2string (pvar env ty)

let print_type env ty =
  doc2string (pty env ty)

let print_equations env eqs =
  doc2string (peqs env eqs)

let print_program p =
  doc2string (print_program p)

