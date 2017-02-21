open Printf
open Parser
open Lexer
open Syntax
open Print

(* ------------------------------------------------------------------------- *)

(* Parse the command line. *)

let filename =
  ref None

let please_echo =
  ref false

let please_typecheck =
  ref false

let please_translate =
  ref false

let usage =
  sprintf "Usage: %s <options> <filename>\n" Sys.argv.(0)

let () =
  Arg.parse (Arg.align [
    "--echo", Arg.Set please_echo, " Prior to typechecking, echo source program";
    "--typecheck", Arg.Set please_typecheck, " Typecheck and display inferred type";
    "--translate", Arg.Set please_translate, " Typecheck, translate, and display translated program";
  ]) (fun name -> filename := Some name) usage

let filename =
  match !filename with
  | Some filename ->
      filename
  | None ->
      fprintf stderr "%s%!" usage;
      exit 1

(* ------------------------------------------------------------------------- *)

(* Read the file; lex; parse; internalize. *)

let prog : Syntax.program =
  let lexbuf = LexerUtil.open_in filename in
  try
    Parser.program Lexer.main lexbuf
  with Parser.Error ->
    Error.errorb lexbuf "Syntax error.\n"

let prog : Terms.program =
  Internalize.program prog

let () =
  if !please_echo then
    printf "%s\n%!" (print_program prog)

(* ------------------------------------------------------------------------- *)

(* Type-check the program. *)

let () =
  if !please_typecheck || !please_translate then
    let xenv, ty = Typecheck.run prog in
    if !please_typecheck then
      printf "%s\n%!" (print_type xenv ty);
    if !please_translate then
      printf "%s\n%!" (print_program (Defunct.translate prog))

