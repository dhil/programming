{

  open Lexing
  open Parser

  (* A table of keywords. *)

  let string2keyword, keyword2string =
    LexerUtil.setup [
      "fun", FUN;
      "let", LET;
      "in", IN;
      "rec", REC;
      "match", MATCH;
      "with", WITH;
      "end", END;
      "forall", FORALL;
      "return", RETURN;
      "data", DATA;
      "constructor", CONSTRUCTOR;
      "program", PROGRAM;
      "where", WHERE;
      "and", AND;
      "type", TYPE;
      "fix", FIX;
    ]

}

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let digit = [ '0'-'9' ]

let integer = ( "0x" | "0o" | "0b" )? digit+

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let identifier = lowercase identchar*

let tag = uppercase identchar*

rule main = parse
| newline
    { LexerUtil.newline lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| identifier as id
    { try
        string2keyword id
      with Not_found ->
	IDENTIFIER (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf)
    }
| tag as id
    { TAG (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) }
| ";"
    { SEMI }
| ":"
    { COLON }
| "="
    { EQ }
| "."
    { DOT }
| "|"
    { BAR }
| "->"
    { ARROW }
| "("
    { LPAR }
| ")"
    { RPAR }
| "{"
    { LBRACE }
| "}"
    { RBRACE }
| "["
    { LBRACKET }
| "]"
    { RBRACKET }
| "(*"
    { comment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| eof
    { EOF }
| _
    { Error.errorb lexbuf "Unexpected character." }

and comment openingp = parse
| "(*"
    { comment (lexeme_start_p lexbuf) lexbuf; comment openingp lexbuf }
| "*)"
    { () }
| newline
    { LexerUtil.newline lexbuf; comment openingp lexbuf }
| eof
    { Error.error [ (openingp, lexeme_start_p lexbuf) ] "Unterminated comment." }
| _
    { comment openingp lexbuf }

