(* ---------------------------------------------------------------------------- *)

(* In order to avoid creating towers of functors, we fix the type of
   sorts: a sort is a pair of an integer (for comparison) and a
   string (for printing). *)

type sort =
    int * string

(* ---------------------------------------------------------------------------- *)

(* Identifiers. *)

(* The identity of an identifier is defined by the pair of its
   name and its sort; see [compare] below. *)

type identifier = {

  (* A textual representation. *)

  name: string;

  (* A sort. *)

  sort: sort;

  (* A position in the source code. *)

  startp: Lexing.position;
  endp: Lexing.position;

}

type t = identifier

(* ---------------------------------------------------------------------------- *)

(* Constructors and accessors. *)

let make name sort startp endp = {
  name = name;
  sort = sort;
  startp = startp;
  endp = endp;
}

let mak sort (name, startp, endp) =
  make name sort startp endp

let mk name sort =
  make name sort Lexing.dummy_pos Lexing.dummy_pos

let name { name = name } = name
let sort { sort = sort } = sort
let startp { startp = startp } = startp
let endp { endp = endp } = endp
let location { startp = startp; endp = endp } = (startp, endp)

(* ---------------------------------------------------------------------------- *)

(* Comparison. *)

let compare { name = name1; sort = (sort1, _) } { name = name2; sort = (sort2, _) } =
  let c = Pervasives.compare (sort1 : int) sort2 in
  if c = 0 then
    String.compare name1 name2
  else
    c

(* ---------------------------------------------------------------------------- *)

(* Maps. *)

module Map = struct

  include Map.Make (struct
    type t = identifier
    let compare = compare
  end)

  let union m1 m2 =
    fold add m2 m1 (* the bindings in [m2] override those in [m1] *)

end
    
(* ---------------------------------------------------------------------------- *)

(* Renaming. *)

(* The subset of ``base'' identifiers is the set of all strings that do not
   end with '_' [ '0'-'9' ]+ '_'. So, [basename] chops off all suffixes of this
   form, while [combine] appends a representation of the integer salt grain to
   the basename. *)

let rec basename (s : string) : string =
  match IdentifierChop.chop (Lexing.from_string s) with
  | Some prefix ->
      basename prefix
  | None ->
      s

let combine (s : string) (i : int) : string =
  if i = 0 then
    s
  else
    s ^ "_" ^ (string_of_int i) ^ "_"

let basename (id : identifier) : identifier =
  { id with name = basename id.name }

let combine (id : identifier) (i : int) : identifier =
  { id with name = combine id.name i }

