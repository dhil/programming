(* ---------------------------------------------------------------------------- *)

(* An atom is an object with unique identity. *)

(* Atoms are heap-allocated. This gives us a number of good properties: the
   number of atoms is limited only by available memory; unused atoms are
   collected; atoms can contain informational fields (for pretty-printing) as
   well as transient mutable fields (for copying). *)

(* In order to support efficient persistent maps over atoms, we still need
   each atom to carry a unique number. With 31 bit integers, we can produce
   two billion atoms. 64 bit integers could be used if necessary. *)

type atom = {

  (* The identifier that originally gave rise to this atom. *)

  identifier: Identifier.t;

  (* This atom's identity. *)

  number: int;

  (* Transient fields. *)

  mutable copy: atom;   (* default: self *)
  mutable next: atom;   (* default: self *)
  mutable copied: bool; (* default: false; only for sanity checking *)

}

(* ---------------------------------------------------------------------------- *)

(* A global counter is used to produce unique numbers. *)

let counter =
  ref 0

let number () : int =
  let number = !counter in
  let successor = number + 1 in
  counter := successor;
  assert (successor <> 0); (* check against overflow *)
  number

(* ---------------------------------------------------------------------------- *)

(* [fresh] and [fresha] produce a fresh atom. *)

let fresh identifier =
  let rec a = {
    identifier = identifier;
    number = number();
    copy = a;
    next = a;
    copied = false;
  } in
  a

let fresha a =
  fresh a.identifier

(* ---------------------------------------------------------------------------- *)

(* Accessors. *)

let identifier { identifier = identifier } = identifier

(* ---------------------------------------------------------------------------- *)

(* Comparison. *)

let equal =
  (==)

let compare a1 a2 =
  Pervasives.compare a1.number a2.number

(* ---------------------------------------------------------------------------- *)

(* Sets and maps over atoms. *)

module Atom = struct
  type t = atom
  let compare = compare
end

module AtomSet =
  Set.Make (Atom)

module AtomMap = struct

  include Map.Make (Atom)

  let of_list kds =
    List.fold_left (fun m (key, data) -> add key data m) empty kds

  let cardinal m =
    fold (fun _ _ n -> n + 1) m 0

  let index m =
    let n, m =
      fold (fun key _ (n, m) ->
	n + 1, add key n m
      ) m (0, empty)
    in
    let index key =
      find key m
    in
    n, index

end

(* ---------------------------------------------------------------------------- *)

(* Copying. *)

(* In order to copy a data structure that contains atoms, one sets up a
   copying phase. One can then invoke [copy_binder] or [copy_occurrence] to
   copy individual atoms. For a given atom [a], there must be at most one call
   to [copy_binder], which must precede any calls to [copy_occurrence]. Once
   the copying phase is over, one must close it. *)

(* The transient field [copy] is used to map each atom to its copy.
   If the atom [a] has not been copied during this copying phase,
   then a fresh atom [a'] is created, whose address is stored in
   [a.copy]. The atom [a] is inserted into a queue -- implemented
   as a circular linked list, via the [next] fields -- which is later
   used to restore all transient fields to their default value. *)

(* The queue header is a dummy atom, and the queue elements are linked
   from it via the [next] fields. (In order to avoid the use of
   options, the queue header points to itself when the queue is
   empty. This forces it to have type [atom].) The last atom in the
   queue points back to the header. (This makes the code more
   uniform.) *)

(* The field [copied] records the fact that a call to [copy_binder] or
   [copy_occurrence] was made, and is used to forbid any subsequent
   calls to [copy_binder]. *)

(* One must be careful to insert each atom at most once in the queue,
   otherwise [close] will fail. Note that an atom [a] appears in the
   queue if and only if [a.copied] is set. *)

type queue =
    atom

type phase =
    queue

let dummy_identifier : Identifier.t =
  Identifier.make
    "__queue_header"
    (0, "")
    Lexing.dummy_pos
    Lexing.dummy_pos

let setup () : queue =
  let rec header : atom = {
    identifier = dummy_identifier;
    number = (-1);
    copy = header;
    next = header; (* this is the only relevant field: this is an empty circular list *)
    copied = false;
  } in
  header

let insert (queue : queue) (a : atom) : unit =
  (* Insert [a] in the circular list, just after the header. *)
  let header = queue in
  let next = header.next in
  header.next <- a;
  a.next <- next

let fold (f : atom -> 'a -> 'a) (queue : queue) (accu : 'a) : 'a =
  let header = queue in
  let rec loop (a : atom) (accu : 'a) : 'a =
    if a == header then
      accu
    else
      let next = a.next in
      let accu = f a accu in
      loop next accu
  in
  loop header.next accu

let copy_binder (queue : queue) (a : atom) : atom =

  (* If the following check fails, then [copy_binder] or [copy_occurrence] was
     invoked before; this is illegal and suggests either incorrect usage of
     these functions by the client, or non-uniqueness of binders. *)

  assert (not a.copied);

  (* If this fails, then the following invariant is violated: when [a.copied]
     is [false], the transient fields of the atom [a] have their default
     values. *)

  assert (a.copy == a && a.next == a); 

  (* Create a fresh atom. Link [a] to it. Insert [a] into the queue. *)

  let rec a' = {
    identifier = a.identifier;
    number = number();
    copy = a';
    next = a';
    copied = false;
  } in
  a.copy <- a';
  a.copied <- true;
  insert queue a;
  a'

let copy_occurrence (queue : queue) (a : atom) : atom =

  (* Set [a.copied], so as to detect an invalid later call to [copy_binder].
     Because we set [a.copied], we must insert [a] into the queue, so that
     [a.copied] is properly reset to [false] when the copying phase is over. *)

  if not a.copied then begin
    a.copied <- true;
    insert queue a
  end;

  (* Regardless of whether a fresh copy of [a] has been created, return the
     contents of its [copy] field. If the binder that corresponds to this
     occurrence lies in the scope of this copying phase, then we get the fresh
     copy. Otherwise, we get [a] itself -- [a] is a ``free atom''. *)

  a.copy

(* [close queue] considers each atom in [queue], and restores its
   transient fields to their default values. The queue itself becomes
   invalid and is discarded. *)

let close (queue : queue) : unit =
  fold (fun a () ->
    a.copy <- a;
    a.next <- a;
    a.copied <- false
  ) queue ()

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

let map_binder (queue : queue) (a : atom) (a' : atom) : unit =

  (* If the following check fails, then [map_binder] or [map_occurrence] was
     invoked before; this is illegal and suggests either incorrect usage of
     these functions by the client, or non-uniqueness of binders. *)

  assert (not a.copied);

  (* If this fails, then the following invariant is violated: when [a.copied]
     is [false], the transient fields of the atom [a] have their default
     values. *)

  assert (a.copy == a && a.next == a); 

  (* Link [a] to [a']. Insert [a] into the queue. *)

  a.copy <- a';
  a.copied <- true;
  insert queue a

let map_occurrence (queue : queue) (a : atom) (a' : atom) : unit =

  (* Set [a.copied], so as to detect an invalid later call to [map_binder].
     Because we set [a.copied], we must insert [a] into the queue, so that
     [a.copied] is properly reset to [false] when the copying phase is over. *)

  if not a.copied then begin
    a.copied <- true;
    insert queue a
  end;

  (* Check that [a'] is the image of [a]. *)

  if a.copy != a' then
    raise Map

(* ---------------------------------------------------------------------------- *)

(* Listing free atoms. *)

let fv_binder (queue : queue) (a : atom) : unit =

  (* If the following check fails, then [fv_binder] or [fv_occurrence] was
     invoked before; this is illegal and suggests either incorrect usage of
     these functions by the client, or non-uniqueness of binders. *)

  assert (not a.copied);

  (* If this fails, then the following invariant is violated: when [a.copied]
     is [false], the transient fields of the atom [a] have their default
     values. *)

  assert (a.copy == a && a.next == a); 

  (* Mark this atom. *)

  a.copied <- true;
  insert queue a

let fv_occurrence (queue : queue) (a : atom) (accu : atom list) : atom list =

  (* If the atom is marked, it is either free, but already known, or bound.
     In either case, it should not be added to the list of free atoms. If
     the atom is unmarked, then it is free and was not previously known:
     add it to the list and mark it. *)

  if not a.copied then begin
    a.copied <- true;
    insert queue a;
    a :: accu
  end
  else
    accu

