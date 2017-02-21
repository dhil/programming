open Atom
open Types
open Terms

(* A few auxiliary functions to help answer questions about programs. *)

(* [fv term] is the set of the free term variables of [term]. *)

val fv: fterm -> AtomSet.t

(* [type_constructor p dc] produces the type constructor with which the
   data constructor [dc] is associated. *)

val type_constructor: program -> atom -> atom

(* [type_scheme p dc] produces the type scheme associated with the data
   constructor [dc]. *)

val type_scheme: program -> atom -> ftype

(* [data_constructors p tc] produces a set of the data constructors
   associated with the type constructor [tc]. *)

val data_constructors: program -> atom -> AtomSet.t

