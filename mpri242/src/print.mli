open Atom
open Types
open Equations
open Terms

(* This module defines a pretty-printer for System F types and terms
   in internal form. It relies on the basic machinery provided by
   [Export]. *)

(* Print an atom. *)

val print_atom: Export.env -> atom -> string

(* Print a type. *)

val print_type: Export.env -> ftype -> string

(* Print a set of type equations. *)

val print_equations: Export.env -> equations -> string

(* Print a program. *)

val print_program: program -> string

