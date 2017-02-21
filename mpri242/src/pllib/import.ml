(* This module provides support for mapping identifiers to atoms. *)

(* Errors about undefined or multiply-defined identifiers are reported
   on [stderr], via the [Error] module. An entire phase can be carried
   out even if errors are found on the way. At the end of the phase,
   it is up to the client to invoke [Error.signaled()], so that,
   if any errors were found, the program is aborted. *)

(* A multiset of identifiers maps an identifier [x] to a list of identifiers
   that are equal to [x] -- in the sense of [Identifier.compare] -- but that
   carry distinct source code locations. This allows reporting
   multiply-defined identifiers in a nice way. *)

type multiset =
    Identifier.t list Identifier.Map.t

let list2multiset (xs : Identifier.t list) : multiset =
  List.fold_left (fun multiset x ->
    let previous : Identifier.t list =
      try
	Identifier.Map.find x multiset
      with Not_found ->
	[]
    in
    Identifier.Map.add x (x :: previous) multiset
  ) Identifier.Map.empty xs

(* When a multiply-defined identifier is detected, an error message is
   displayed, which carries the name of the identifier, together with a
   list of source code locations where this identifier is defined. *)

let allocate (xs : Identifier.t list) : Atom.atom =
  match xs with
  | [] ->
      assert false  (* cannot happen *)
  | [ x ] ->
      Atom.fresh x (* normal case; allocate a fresh atom for [x] *)
  | x :: _ :: _ ->
      Error.signal
	(List.map Identifier.location xs)
	(Printf.sprintf "Multiply-defined %s identifier: %s.\n"
	  (let _, sort = Identifier.sort x in sort)
	  (Identifier.name x));
      Atom.fresh x (* abnormal case; succeed anyway, for now *)

(* An import environment maps identifiers to atoms. *)

type env =
    Atom.atom Identifier.Map.t

(* The empty environment. *)

let empty : env =
  Identifier.Map.empty

(* Contanenation of environments. The bindings in the right-hand
   environment take precedence. *)

let cat =
  Identifier.Map.union

(* [fragment xs] turns a list of identifiers, which are to be newly
   bound, into an environment fragment that maps each identifier in
   [xs] to a fresh atom. A check against multiply-defined identifiers
   is performed. The power of this operation can be necessary for
   certain complex binding structures; however, in most common cases,
   the [bind] functions below, which extend a pre-existing
   environment, are sufficient. *)

(* Turn the list [xs] of identifiers into a multiset. Then, collapse
   the multiset, to obtain a map of identifiers to atoms. In the
   process, we check for multiply-defined identifiers. *)

let fragment xs : env =
  Identifier.Map.map allocate (list2multiset xs)

(* [linear xs] checks the list [xs] against multiply-defined identifiers. *)

let linear xs : unit =
  let (_ : env) = fragment xs in
  ()

(* [bind_simultaneously env xs] extends the pre-existing environment
   [env] with the new environment [fragment xs]. *)

let bind_simultaneously env xs : env =
  cat env (fragment xs)

(* [bind env x] is equivalent to [bind_simultaneously env [x]]. *)

let bind env x : env =
  Identifier.Map.add x (Atom.fresh x) env

(* [obind] is analogous to [bind], but concerns an optional identifier. *)

let obind env = function
  | None ->
      env
  | Some x ->
      bind env x

(* [bind_sequentially env xs] binds the sequence of identifiers [xs], one
   after the other. It returns a pair of an extended environment and of a list
   of the atoms that were chosen fresh. (Hence, this is a list of distinct
   atoms, even if [xs] contains duplicate identifiers.) *)

(* Be careful: we can't just first iterate [bind] to obtain a new environment
   [env], then iterate [resolve env]. Indeed, a new binding could hide a
   previous one. We must really build the list of atoms as we go. This is more
   efficient, anyway. *)

let bind_sequentially env xs : env * Atom.atom list =
  let env, atoms =
    List.fold_left (fun (env, atoms) x ->
      let atom = Atom.fresh x in
      Identifier.Map.add x atom env,
      atom :: atoms
    ) (env, []) xs
  in
  env, List.rev atoms

(* [resolves env sort x] tells whether the identifier [Identifier.mak sort x]
   is bound in the environment [env]. That is, it tells whether [resolve]
   will succeed if [x] is considered as a name of sort [sort]. *)

let resolves env sort x =
  Identifier.Map.mem (Identifier.mak sort x) env

(* [resolve env x] looks up the identifier [x] in the environment [env]. If
   [x] is unknown, an error is signaled. *)

let resolve env x : Atom.atom =
  try
    Identifier.Map.find x env (* normal case: return the atom associated with [x] *)
  with Not_found ->
    Error.signal
      [ Identifier.location x ]
      (Printf.sprintf "Undefined %s identifier: %s.\n"
	(let _, sort = Identifier.sort x in sort)
	(Identifier.name x));
    Atom.fresh x             (* abnormal case; succeed anyway, for now *)

(* Sometimes, identifiers of a certain sort are bound globally and implicitly
   -- that is, there is no explicit binding form for them. In that case, a
   mutable, global environment must be used to map identifiers to atoms. The
   call [mkglobal()] creates such a global environment, and returns a function
   that turns identifiers to atoms. *)

let mkglobal () : Identifier.t -> Atom.atom =
  let env =
    ref empty
  in
  fun id ->
    try
      Identifier.Map.find id !env
    with Not_found ->
      let atom = Atom.fresh id in
      env := Identifier.Map.add id atom !env;
      atom

