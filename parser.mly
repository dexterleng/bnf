%{ 
  
%}

/* File parser.mly */
%token <string> NON_TERMINAL TERMINAL
%token EOF
%token OR
%token ASSIGN
%token NEWLINE
%start main             /* the entry point */
%type <Types.assignment list> main
%%

main:
  line main { $1::$2 }
  | EOF { [] }

line:
  assignment NEWLINE { $1 }
  | NEWLINE { Types.Nothing }
;

assignment:
  NON_TERMINAL ASSIGN expr { Types.Assignment(Types.NonTerminal($1), $3) }
;

expr:
  either_terminal exprP { $1::$2 }
;

exprP:
  epsilon { [] }
  | either_terminal exprP { $1::$2 }
;

either_terminal:
  NON_TERMINAL { Types.Left(Types.NonTerminal $1) }
 | TERMINAL { Types.Right(Types.Terminal $1) }
;

epsilon:
  { [] }