(* This module helps report errors. *)

type location =
    Lexing.position * Lexing.position

(* A dummy location. *)

val dummy: location

(* [override loc1 loc2] returns [loc2], unless [loc2] is a dummy location,
   in which case it returns [loc1]. *)

val override: location -> location -> location

(* [error locs msg] displays the error message [msg], referring to the
   locations [locs], and stops the program. *)

val error: location list -> string -> 'a

(* [errora] is analogous to [error], but accepts a list of atoms instead
   of a list of locations. The locations carried by these atoms are used. *)

val errora: Atom.atom list -> string -> 'a

(* [errorb] is analogous to [error], but accepts a lexing buffer. *)

val errorb: Lexing.lexbuf -> string -> 'a

(* [signal locs msg] is analogous to [error], but does not immediately stop
   the program. In order to stop the program, one invokes [signaled()], which
   stops the program if any call to [signal] was previously issued. *)

val signal: location list -> string -> unit
val signala: Atom.atom list -> string -> unit
val signaled: unit -> unit

(* [warning locs msg] is analogous to [error] and [signal], but has no
   effect other than printing a message. *)

val warning: location list -> string -> unit

