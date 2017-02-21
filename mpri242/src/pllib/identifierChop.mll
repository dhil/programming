(* This lexer is used by [Identifier]. *)

rule chop = parse
| (_* as prefix) '_' [ '0'-'9' ]+ '_' eof
    { Some prefix }
| _* eof
    { None }

