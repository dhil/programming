open Atom
open Identifier

(* This module provides support for mapping identifiers to atoms. *)

(* Errors about undefined or multiply-defined identifiers are reported
   on [stderr], via the [Error] module. An entire phase can be carried
   out even if errors are found on the way. At the end of the phase,
   it is up to the client to invoke [Error.signaled()], so that,
   if any errors were found, the program is aborted. *)

(* An import environment maps identifiers to atoms. *)

type env

(* The empty environment. *)

val empty: env

(* Contanenation of environments. The bindings in the right-hand
   environment take precedence. *)

val cat: env -> env -> env

(* [fragment xs] turns a list of identifiers, which are to be newly
   bound, into an environment fragment that maps each identifier in
   [xs] to a fresh atom. A check against multiply-defined identifiers
   is performed. The power of this operation can be necessary for
   certain complex binding structures; however, in most common cases,
   the [bind] functions below, which extend a pre-existing
   environment, are sufficient. *)

val fragment: identifier list -> env

(* [linear xs] checks the list [xs] against multiply-defined identifiers. *)

val linear: identifier list -> unit

(* [bind_simultaneously env xs] extends the pre-existing environment
   [env] with the new environment [fragment xs]. *)

val bind_simultaneously: env -> identifier list -> env
  
(* [bind env x] is equivalent to [bind_simultaneously env [x]]. *)

val bind: env -> identifier -> env

(* [obind] is analogous to [bind], but concerns an optional identifier. *)

val obind: env -> identifier option -> env

(* [bind_sequentially env xs] binds the sequence of identifiers [xs], one
   after the other. It returns a pair of an extended environment and of a list
   of the atoms that were chosen fresh. (Hence, this is a list of distinct
   atoms, even if [xs] contains duplicate identifiers.) *)

val bind_sequentially: env -> identifier list -> env * atom list

(* [resolves env sort x] tells whether the identifier [Identifier.mak sort x]
   is bound in the environment [env]. That is, it tells whether [resolve]
   will succeed if [x] is considered as a name of sort [sort]. *)

val resolves: env -> sort -> string * Lexing.position * Lexing.position -> bool

(* [resolve env x] looks up the identifier [x] in the environment [env].
   If [x] is unknown, an error is signaled. *)

val resolve: env -> identifier -> atom

(* Sometimes, identifiers of a certain sort are bound globally and implicitly
   -- that is, there is no explicit binding form for them. In that case, a
   mutable, global environment must be used to map identifiers to atoms. The
   call [mkglobal()] creates such a global environment, and returns a function
   that turns identifiers into atoms. *)

val mkglobal: unit -> (identifier -> atom)

