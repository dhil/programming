(* This module provides support for mapping atoms back to identifiers,
   while avoiding capture. *)

type identifier =
  Identifier.t

module AM =
  Atom.AtomMap

module IM =
  Identifier.Map

(* An environment contains a mapping of atoms to identifiers. Furthermore,
   because it is necessary to efficiently determine whether an identifier is
   already used, an environment contains a mapping of base identifiers to
   integers. If a base identifier [basename] is mapped to an integer [grain],
   then the identifier [Identifier.combine basename grain] is available (and
   so is every identifier above it). *)

type env = {
  mapping: identifier AM.t;
  salt: int IM.t;
}

(* The empty environment. *)

let empty = {
  mapping = AM.empty;
  salt = IM.empty;
}

(* [bind env a] extends the environment [env] with a mapping of
   the atom [a] to a unique identifier. *)

let bind { mapping = mapping; salt = salt } a =
  let basename : identifier =
    Identifier.basename (Atom.identifier a)
  in
  let grain : int=
    try
      IM.find basename salt
    with Not_found ->
      0
  in
  let id : identifier =
    Identifier.combine basename grain
  in
  {
    mapping = AM.add a id mapping;
    salt = IM.add basename (grain + 1) salt
  }

let obind env = function
  | None ->
      env
  | Some a ->
      bind env a

let sbind env atoms =
  List.fold_left bind env atoms

(* [resolve env a] looks up the identifier associated with [a] in
   the environment [env]. It is an error to look up an identifier
   that was not previously bound. *)

(* In other words, if your code causes this assertion to fail, then
   you probably forgot to bind some type variables in your export
   environment! *)

let resolve { mapping = mapping } a =
  try
    AM.find a mapping
  with Not_found ->
    Printf.fprintf stderr "Export: unbound atom: %s\n" (Identifier.name (Atom.identifier a));
    assert false

(* Sometimes, identifiers of a certain sort are bound globally and implicitly
   -- that is, there is no explicit binding form for them. In that case, a
   mutable, global environment must be used to map atoms to identifiers. The
   call [mkglobal()] creates such a global environment, and returns a function
   that turns atoms into identifiers. *)

let mkglobal () =
  let env =
    ref empty
  in
  fun a ->
    try
      AM.find a (!env).mapping
    with Not_found ->
      env := bind !env a;
      resolve !env a

