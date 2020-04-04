/* File parser.mly */
%token <string> NON_TERMINAL TERMINAL
%token OR
%token ASSIGN
%token NEWLINE
%start main             /* the entry point */
%type <int> main
%%

main:
  line main { 1 }

line:
  assignment NEWLINE { 1 }
  | NEWLINE { 1 }
;

assignment:
  NON_TERMINAL ASSIGN expr { 1 }
;

expr:
  either_terminal exprP { 1 }
;

exprP:
  epsilon { 1 }
  | either_terminal exprP { 1 }
;

either_terminal:
  NON_TERMINAL { 1 }
 | TERMINAL { 1 }
;

epsilon:
  { 1 }