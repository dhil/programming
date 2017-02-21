open Printf
open Lexing

type location =
    Lexing.position * Lexing.position

let dummy =
  (Lexing.dummy_pos, Lexing.dummy_pos)

let is_dummy (pos1, pos2) =
  pos1 == Lexing.dummy_pos && pos2 == Lexing.dummy_pos

let override loc1 loc2 =
  if is_dummy loc2 then loc1 else loc2

let print_location (pos1, pos2) =
  let file = pos1.pos_fname in
  let line = pos1.pos_lnum in
  let char1 = pos1.pos_cnum - pos1.pos_bol in
  let char2 = pos2.pos_cnum - pos1.pos_bol in (* intentionally [pos1.pos_bol] *)
  fprintf stderr "File \"%s\", line %d, characters %d-%d:\n" file line char1 char2
    (* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)

let signaled =
  ref false

let atoms2locs atoms =
  List.map (fun a ->
    let id = Atom.identifier a in
    Identifier.startp id, Identifier.endp id
  ) atoms

let warning locs message =
  List.iter print_location locs;
  fprintf stderr "%s%!" message

let signal locs message =
  warning locs message;
  signaled := true

let signala atoms message =
  signal (atoms2locs atoms) message

let error locs message =
  signal locs message;
  exit 1

let errora atoms message =
  error (atoms2locs atoms) message

let errorb lexbuf msg =
  error [ (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) ] msg

let signaled () =
  if !signaled then
    exit 1

