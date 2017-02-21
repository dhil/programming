%{

open List
open Syntax

let distribute (xs : 'x list) (y : 'y) : ('x * 'y) list =
  map (fun x -> (x, y)) xs

let abs te_args t =
  fold_right (fun (x, ty) t -> SynTeAbs (x, ty, t)) te_args t

let tyabs ty_args t =
  fold_right (fun a t -> SynTeTyAbs (a, t)) ty_args t

let tyapp t tys =
  fold_left (fun t ty -> SynTeTyApp (t, ty)) t tys

let otyannot t = function
  | None ->
      t
  | Some ty ->
      SynTeTyAnnot (t, ty)

let arrows te_args ty =
  fold_right (fun (_, domain) codomain -> SynTyArrow (domain, codomain)) te_args ty

let forall tvars ty =
  fold_right (fun tv ty -> SynTyForall (tv, ty)) tvars ty

let compose f g t =
  f (g t)

let identity t =
  t

let make_non_recursive_definition ty_args te_args ocodomain t =
  (* if there are no arguments at all, then this is not a function *)
  (* this special case is required to avoid ambiguity between the
     primitive form [let x = t1 in t2] and the syntactic sugar
     [let f (...) = t1 in t2], in the particular case where there
     are no arguments. *)
  tyabs ty_args (
    abs te_args (
      otyannot t ocodomain
    )
  )

let make_recursive_definition f loc ty_args te_args codomain t =
  (* if there are no arguments at all, then this is not a function *)
  (* this special case is required to avoid ambiguity between the
     primitive form [let rec x = t1 in t2] and the syntactic sugar
     [let rec f (...) = t1 in t2], in the particular case where there
     are no arguments. *)
  SynTeFix (
    f,
    forall ty_args (arrows te_args codomain),
    SynTeLoc (loc,
      make_non_recursive_definition ty_args te_args (Some codomain) t
    )
  )

let name_of_fix = function
  | SynTeFix (f, _, _) ->
      f
  | _ ->
      assert false (* cannot happen *)

%}

%token <string * Lexing.position * Lexing.position> IDENTIFIER TAG
%token SEMI COLON EQ DOT BAR ARROW
%token LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET
%token FUN LET IN REC MATCH WITH END FORALL RETURN DATA CONSTRUCTOR PROGRAM WHERE AND TYPE FIX
%token EOF
%start <Syntax.program> program

%%

(* ------------------------------------------------------------------------- *)

(* In ocaml syntax, SEMI can be used either as a delimiter or as a
   terminator. That is, the last SEMI is optional. Here is a way of
   saying so. *)

semi(X):
| (* nothing *)
    { [] }
| xs = terminated(X, SEMI)+
| xs = separated_nonempty_list(SEMI, X)
    { xs }

(* ------------------------------------------------------------------------- *)

(* Similarly, BAR can be used either as a delimiter or as an opening symbol.
   That is, the first BAR is optional. *)

bar(X):
| (* nothing *)
    { [] }
| xs = preceded(BAR, X)+
| xs = separated_nonempty_list(BAR, X)
    { xs }

(* ------------------------------------------------------------------------- *)

(* Identifiers. *)

(* Each identifier is tagged with its sort, which is known thanks
   to the context where the identifier appears. *)

(* Data constructors must begin with an uppercase letter, so the lexer
   distinguishes them. Not drawing this distinction would take us beyond
   LR(1). *)

term_variable:
| id = IDENTIFIER
    { Identifier.mak term_sort id }

type_variable:
| id = IDENTIFIER
    { Identifier.mak type_sort id }

data_constructor:
| id = TAG
    { Identifier.mak data_sort id }

type_constructor:
| id = IDENTIFIER
    { Identifier.mak typecon_sort id }

(* ------------------------------------------------------------------------- *)

(* Types. *)

typ0:
| id = IDENTIFIER
    (* this could be a type variable or type constructor *)
    { SynTyVarOrTyCon (id, []) }
| LPAR ty = typ RPAR
    { ty }

typ1:
| id = IDENTIFIER params = typ0+
    (* this should be a type constructor, since it has parameters,
       but we leave the ambiguity for the sake of uniformity *)
    { SynTyVarOrTyCon (id, params) }
| ty = typ0
    { ty }

typ:
| ty = typ1
    { ty }
| ty1 = typ1 ARROW ty2 = typ
    { SynTyArrow (ty1, ty2) }
| FORALL tvars = type_variable* DOT ty = typ
    { forall tvars ty }

(* ------------------------------------------------------------------------- *)

(* Type schemes for data constructors. *)

quantifiers:
| FORALL tvars = type_variable* DOT
    { tvars }

equation:
| ty1 = typ EQ ty2 = typ
    { SynEq (ty1, ty2) }

where_or_and:
| WHERE
| AND
    { () }

equations:
| WHERE eqs = separated_nonempty_list(where_or_and, equation)
    { eqs }

scheme:
| tvars = loption(quantifiers)
  LBRACE fields = semi(typ) RBRACE
  ARROW tc = type_constructor params = typ0*
  eqs = loption(equations)
    { SynScheme (tvars, eqs, fields, tc, params) }

datacon_signature:
| DATA CONSTRUCTOR d = data_constructor COLON s = scheme
    { SynDatacon (d, s) }

(* ------------------------------------------------------------------------- *)

(* Type declarations. *)

type_signature:
| TYPE tc = type_constructor params = type_variable*
    { SynType (tc, params) }

(* ------------------------------------------------------------------------- *)

(* Function arguments. *)

(* Type variables are surrounded with square brackets. Term variables are
   surrounded with parentheses and must carry a type annotation. For simplicity,
   we require that type arguments come first. *)

(* Formal type arguments must be type variables. Actual type arguments may be
   arbitrary types. *)

(* In order to avoid ambiguity, we restrict actual type arguments to only one
   argument per bracket pair. For the sake of uniformity, we impose the same
   restriction upon formal type arguments. *)

formal_type_arguments:
| LBRACKET tvar = type_variable RBRACKET (* could allow type_variable+ *)
    { [ tvar ] }

actual_type_arguments:
| LBRACKET tys = typ RBRACKET (* could instead allow typ0+ *)
    { [ tys ] }

term_arguments:
| LPAR xs = term_variable+ COLON domain = typ RPAR
    { distribute xs domain }

%inline multiple(X):
| xs = X*
    { concat xs }

(* ------------------------------------------------------------------------- *)

(* Function definitions. *)

(* The function definition follows either [FUN] or [LET f] or [LET REC]. *)

(* If the definition begins with a function name, then the function is considered
   recursive, and the result type annotation is mandatory. Otherwise, the
   function is considered non-recursive, and the result type annotation is
   optional. *)

non_recursive_def:
| ty_args = multiple(formal_type_arguments)
  te_args = multiple(term_arguments)
  codomain = preceded(COLON,typ)?
  EQ t = loc(term)
    { make_non_recursive_definition ty_args te_args codomain t }

recursive_def:
| f = term_variable
  ty_args = multiple(formal_type_arguments)
  te_args = multiple(term_arguments)
  codomain = preceded(COLON,typ)
  EQ t = loc(term)
    { let loc = ($startpos(ty_args), $endpos(t)) in
      make_recursive_definition f loc ty_args te_args codomain t }
    (* one should check that the right-hand side of a recursive definition
       is a value, e.g. a function or a data constructor application; this
       is not done for the moment. *)

def:
| def = non_recursive_def
| def = recursive_def
    { def }

(* ------------------------------------------------------------------------- *)

(* Terms. *)

term0:
| x = term_variable
    { SynTeVar x }
| d = data_constructor tys = multiple(actual_type_arguments) LBRACE fields = semi(loc(term)) RBRACE
    { SynTeData (d, tys, fields) }
| LPAR t = term RPAR
    { t }
| LPAR t = loc(term) COLON ty = typ RPAR
    { SynTeTyAnnot (t, ty) }
| MATCH t = loc(term) RETURN ty = typ WITH cs = bar(clause) END
    { SynTeMatch (t, ty, cs) }

term1:
| t = term0
    { t }
| t1 = loc(term1) t2 = loc(term0)
    { SynTeApp (t1, t2) }
| t = loc(term1) tys = actual_type_arguments (* no [multiple], that would be ambiguous *)
    { tyapp t tys }

term:
| t = term1
    { t }
| FUN def = def
    { def }
| FIX f = term_variable COLON ty = typ EQ t = loc(term)
    (* the syntax of [fix] is chosen so that [fix] can always be replaced
       by [fun] -- it looks just like a recursive function of arity 0. *)
    (* one should check that the right-hand side of a recursive definition
       is a value, e.g. a function or a data constructor application; this
       is not done for the moment. *)
    { SynTeFix (f, ty, t) }

(* The following form could have been primitive, but is subsumed by the
   syntactic sugar that follows.

| LET x = term_variable EQ t1 = loc(term) IN t2 = loc(term)
    { SynTeLet (x, t1, t2) }

*)

(* Syntactic sugar: [let] forms for function definitions. *)

| LET f = term_variable def = non_recursive_def IN t = loc(term)
    { SynTeLet (f, def, t) }
| LET REC def = recursive_def IN t = loc(term)
    { let f = name_of_fix def in
      SynTeLet (f, def, t) }

(* ------------------------------------------------------------------------- *)

(* Case analyses: clauses and patterns. *)

clause:
| p = pattern ARROW t = loc(term)
    { SynClause (p, t) }

pattern:
| d = data_constructor tvars = multiple(formal_type_arguments) LBRACE fields = semi(term_variable) RBRACE
    { SynPatData (($startpos, $endpos), d, tvars, fields) }

(* ------------------------------------------------------------------------- *)

(* Recording locations. *)

loc(TERM):
| t = TERM
    { SynTeLoc (($startpos, $endpos), t) }

(* ------------------------------------------------------------------------- *)

(* Programs. *)

signature_item:
| d = datacon_signature
| d = type_signature
    { d }

program:
| ds = signature_item* PROGRAM t = loc(term) EOF
    { SynProg (ds, t) }

