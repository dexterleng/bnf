{
  open Parser

  (* exception Eof *)

  let line_number = ref 1
}

let assign = "::="

let letter = ['a'-'z' 'A'-'Z']
let letterWithUnderscore = ['a'-'z' 'A'-'Z' '_']
let name = letter letterWithUnderscore*

let whitespace = [' ' '\t']
let newline = '\n'

let or = '|'

let terminal_name = name
let non_terminal_name = '<' name '''* '>'
let epsilon = "#e"

let non_terminal_expr = non_terminal_name assign (terminal_name | non_terminal_name)+ newline

rule token = parse
  | whitespace      { token lexbuf }
  | newline         { incr line_number; NEWLINE }
  | assign          { ASSIGN }
  | or              { OR }
  | terminal_name as name            { TERMINAL name }
  | non_terminal_name as name    { NON_TERMINAL (String.sub name 1 ((String.length name) - 2)) }
  | epsilon { EPSILON }
  | '(' { LBRACE }
  | ')' { RBRACE }
  | '|' { OR }
  | eof		          { EOF }

