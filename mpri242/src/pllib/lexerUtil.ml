open Lexing

(* [open_in filename] opens the specified file and produces a lexing buffer
   whose positions refer to [filename]. *)

let open_in filename =
  let lexbuf = from_channel (open_in filename) in
  lexbuf.lex_curr_p <- { 
    pos_fname = filename; 
    pos_lnum  = 1;
    pos_bol   = 0; 
    pos_cnum  = 0
  };
  lexbuf

(* [newline lexbuf] increments the line counter stored within [lexbuf]. *)

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

(* [setup sts] accepts a list of pairs of a string and a token. It sets up a
   pair of tables that translate (both ways) between strings and tokens, and
   returns functions that look up these tables. *)

open Hashtbl

let setup sts =
  let direct = create 123
  and reverse = create 123 in
  List.iter (fun (s, t) ->
    add direct s t;
    add reverse t s
  ) sts;
  find direct, find reverse

