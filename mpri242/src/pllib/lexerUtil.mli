(* [open_in filename] opens the specified file and produces a lexing buffer
   whose positions refer to [filename]. *)

val open_in: string -> Lexing.lexbuf

(* [newline lexbuf] increments the line counter stored within [lexbuf]. *)

val newline: Lexing.lexbuf -> unit

(* [setup sts] accepts a list of pairs of a string and a token. It sets up a
   pair of tables that translate (both ways) between strings and tokens, and
   returns functions that look up these tables (and can raise [Not_found]). *)

val setup: ('string * 'token) list -> ('string -> 'token) * ('token -> 'string)

