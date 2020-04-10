type non_terminal = string [@@deriving show]
type terminal = string [@@deriving show]

type term =
  | NonTerminal of non_terminal
  | Terminal of terminal
  | Epsilon [@@deriving show]

type

syntax = line list [@@deriving show]

and

line = assignment option [@@deriving show]

and

assignment = { lhs: non_terminal; rhs: expr  } [@@deriving show]

and

expr = or_expr [@@deriving show]

and

or_expr =
  | OR_EXPR of sequential_expr * or_expr
  | OR_EXPR_BASE of sequential_expr [@@deriving show]

and

sequential_expr =
  | SEQUENTIAL_EXPR of primary_expr * sequential_expr
  | SEQUENTIAL_EXPR_BASE of primary_expr [@@deriving show]

and

primary_expr =
  | PRIMARY_EXPR of term
  | PRIMARY_PARENTHESIZED_EXPR of expr [@@deriving show]
