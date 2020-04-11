open Core

type non_terminal = string [@@deriving show, sexp]
type terminal = string [@@deriving show, sexp]

type term =
  | NonTerminal of non_terminal
  | Terminal of terminal
  | Epsilon [@@deriving show]

type first_set_element =
  | Terminal of terminal
  | Epsilon [@@deriving show, sexp]

module FirstSetElement = struct
  type t = first_set_element
  let compare a b = match (a, b) with
    | (Terminal(a), Terminal(b)) -> String.compare a b
    | (Terminal(_), Epsilon) -> 1
    | (Epsilon, Terminal(_)) -> -1
    | (Epsilon, Epsilon) -> 0
  let sexp_of_t t = sexp_of_first_set_element t
  let t_of_sexp t = first_set_element_of_sexp t
end

module FirstSet = Set.Make(FirstSetElement)

let show_first_set s = String.concat ~sep: ", " (List.map (FirstSet.to_list s) ~f:(show_first_set_element))

type follow_set_element =
  | Terminal of terminal
  | EndSymbol [@@deriving show, sexp]

module FollowSetElement = struct
  type t = follow_set_element
  let compare a b = match (a, b) with
    | (Terminal(a), Terminal(b)) -> String.compare a b
    | (Terminal(_), EndSymbol) -> 1
    | (EndSymbol, Terminal(_)) -> -1
    | (EndSymbol, EndSymbol) -> 0
  let sexp_of_t t = sexp_of_follow_set_element t
  let t_of_sexp t = follow_set_element_of_sexp t
end

module FollowSet = Set.Make(FollowSetElement)

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
