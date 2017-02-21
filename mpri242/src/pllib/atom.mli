(* ---------------------------------------------------------------------------- *)

(* An atom is an object with unique identity. *)

type atom

(* ---------------------------------------------------------------------------- *)

(* [fresh] and [fresha] produce a fresh atom. *)

val fresh: Identifier.t -> atom
val fresha: atom -> atom

(* ---------------------------------------------------------------------------- *)

(* Accessors. *)

val identifier: atom -> Identifier.t

(* ---------------------------------------------------------------------------- *)

(* Comparison. *)

val equal: atom -> atom -> bool
val compare: atom -> atom -> int

(* ---------------------------------------------------------------------------- *)

(* Sets and maps over atoms. *)

module AtomSet : Set.S with type elt = atom

module AtomMap : sig

  include Map.S with type key = atom

  val of_list: (atom * 'a) list -> 'a t

  val cardinal: 'a t -> int
  val index: 'a t -> int * (key -> int)

end

(* ---------------------------------------------------------------------------- *)

(* Copying. *)

(* In order to copy a data structure that contains atoms, one sets up a
   copying phase. One can then invoke [copy_binder] or [copy_occurrence] to
   copy individual atoms. For a given atom [a], there must be at most one call
   to [copy_binder], which must precede any calls to [copy_occurrence]. Once
   the copying phase is over, one must close it. *)

type phase

val setup: unit -> phase
val copy_binder: phase -> atom -> atom
val copy_occurrence: phase -> atom -> atom
val close: phase -> unit

(* ---------------------------------------------------------------------------- *)

(* Checking for alpha-equivalence. *)

(* Checking whether two data structures are alpha-equivalent is largely
   analogous to copying a data structure. After all, we are checking
   whether the second data structure could be the result of copying the
   first one. *)

(* For this reason, the comparison mechanism is largely similar to the copy
   mechanism. As above, a comparison phase must be initially set up and
   eventually closed. Furthermore, instead of [copy_binder phase a] and
   [copy_occurrence phase a], we now have [map_binder phase a a'] and
   [map_occurrence phase a a']. The former adds an edge from [a] to [a']. The
   latter checks that an edge from [a] to [a'] exists. If this check fails,
   [Map] is raised. *)

exception Map

(* val setup: as above *)
val map_binder: phase -> atom -> atom -> unit
val map_occurrence: phase -> atom -> atom -> unit
(* val close: as above *)

(* ---------------------------------------------------------------------------- *)

(* Listing free atoms. *)

(* The mechanism for accumulating free atoms is, again, largely similar to the
   above two mechanisms. As above, a phase must be initially set up and
   eventually closed. [fv_binder phase a] marks [a] as bound. [fv_occurrence
   phase a accu] adds the atom [a] to the accumulator [accu] if [a] is a free
   atom and if this is the first time [a] is encountered. *)

(* val setup: as above *)
val fv_binder: phase -> atom -> unit
val fv_occurrence: phase -> atom -> atom list -> atom list
(* val close: as above *)

