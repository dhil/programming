open Atom
open Identifier

(* This module provides support for mapping atoms back to identifiers,
   while avoiding capture. *)

(* An environment is a mapping of atoms to unique identifiers. *)

type env

(* The empty environment. *)

val empty: env

(* [bind env a] extends the environment [env] with a mapping of
   the atom [a] to a unique identifier. [obind] and [sbind] are
   analogous, but respectively concern an optional atom and a
   sequence of atoms. *)

val bind: env -> atom -> env
val obind: env -> atom option -> env
val sbind: env -> atom list -> env

(* [resolve env a] looks up the identifier associated with [a] in
   the environment [env]. It is an error to look up an identifier
   that was not previously bound. *)

val resolve: env -> atom -> identifier

(* Sometimes, identifiers of a certain sort are bound globally and implicitly
   -- that is, there is no explicit binding form for them. In that case, a
   mutable, global environment must be used to map atoms to identifiers. The
   call [mkglobal()] creates such a global environment, and returns a function
   that turns atoms into identifiers. *)

val mkglobal: unit -> (atom -> identifier)

