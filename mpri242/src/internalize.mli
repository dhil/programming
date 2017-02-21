(* This module turns external syntax ito internal syntax. In particular,
   this involves replacing identifiers with atoms, and making sure that
   every identifier is properly bound. *)

val program: Syntax.program -> Terms.program

