type token =
  | NON_TERMINAL of (string)
  | TERMINAL of (string)
  | EPSILON
  | EOF
  | OR
  | ASSIGN
  | NEWLINE
  | LBRACE
  | RBRACE

val syntax :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.syntax
