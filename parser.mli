type token =
  | NON_TERMINAL of (string)
  | TERMINAL of (string)
  | EOF
  | OR
  | ASSIGN
  | NEWLINE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.assignment list
