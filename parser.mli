type token =
  | NON_TERMINAL of (string)
  | TERMINAL of (string)
  | OR
  | ASSIGN
  | NEWLINE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
