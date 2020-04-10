%{ 
  
%}

/* File parser.mly */
%token <string> NON_TERMINAL TERMINAL
%token EPSILON
%token EOF
%token OR
%token ASSIGN
%token NEWLINE
%token LBRACE RBRACE
%start syntax             /* the entry point */
%type <Types.syntax> syntax
%%

syntax:
  line syntax { $1::$2 }
  | EOF { [] }

line:
  assignment NEWLINE { Some($1) }
  | NEWLINE { None }
;

assignment:
  NON_TERMINAL ASSIGN expr { { lhs = $1; rhs = $3 } }
;

expr:
  or_expr { $1 }
;

or_expr:
  sequential_expr OR or_expr { Types.OR_EXPR($1, $3) }
  | sequential_expr { Types.OR_EXPR_BASE($1) }

sequential_expr:
  primary_expr sequential_expr { Types.SEQUENTIAL_EXPR($1, $2) }
  | primary_expr { Types.SEQUENTIAL_EXPR_BASE($1) }
;

primary_expr:
  term { Types.PRIMARY_EXPR($1) }
  | LBRACE expr RBRACE { Types.PRIMARY_PARENTHESIZED_EXPR($2) }

term:
  NON_TERMINAL { Types.NonTerminal($1) }
 | TERMINAL { Types.Terminal($1) }
 | EPSILON { Types.Epsilon }
;
