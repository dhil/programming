(* This module provides an implementation of identifiers. *)

(* ---------------------------------------------------------------------------- *)

(* In order to avoid creating towers of functors, we fix the type of
   sorts: a sort is a pair of an integer (for comparison) and a
   string (for printing). *)

type sort =
    int * string

(* ---------------------------------------------------------------------------- *)

(* An identifier is essentially a pair of a string and a sort. An
   identifier also carries a pair of source code positions, but
   these do not affect the identifier's identity. *)

type identifier
type t = identifier

(* ---------------------------------------------------------------------------- *)

(* Constructors and accessors. *)

val make:
  string ->
  sort ->
  Lexing.position ->
  Lexing.position ->
  t

val mak:
  sort ->
  string * Lexing.position * Lexing.position ->
  t

val mk:
  string ->
  sort ->
  t

val name: t -> string
val sort: t -> sort
val startp: t -> Lexing.position
val endp: t -> Lexing.position
val location: t -> Lexing.position * Lexing.position

(* ---------------------------------------------------------------------------- *)

(* Comparison. *)

val compare: t -> t -> int

(* ---------------------------------------------------------------------------- *)

(* Maps whose keys are identifiers. *)

module Map : sig

  include Map.S with type key = t

  val union: 'a t -> 'a t -> 'a t

end

(* ---------------------------------------------------------------------------- *)

(* A strict subset of the identifiers is referred to as the ``base''
   identifiers. There exists a function, referred to as [basename],
   that maps arbitrary identifiers to base identifiers. This
   function does not have to (and usually cannot) be injective. (A
   function is injective when it maps distinct inputs to distinct
   outputs.) Nevertheless, the more information it preserves, the
   better. Conversely, there exists an injective function, referred
   to as [combine], that maps a pair of a base identifier and an
   integer value to an identifier. It is desirable for the following
   two laws be satisfied:

   \begin{center}
   [basename (combine identifier i) = identifier] \\
   [combine identifier 0 = identifier]
   \end{center}

   The first law states that the information added by combining an
   identifier with an integer [i] is exactly the information that
   is lost when applying [basename]. The second law states that
   combining the integer 0 with an identifier should have no effect.
   These laws may be exploited to avoid needless renamings. *)

val basename: t -> t
val combine: t -> int -> t

