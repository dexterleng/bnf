open Core

type non_terminal = string [@@deriving show, sexp]
type terminal = string [@@deriving show, sexp]

type term =
  | NonTerminal of non_terminal
  | Terminal of terminal [@@deriving show, sexp]

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

module FirstSet = Set.Make(FirstSetElement) [@@deriving show]

let show_first_set s = String.concat ~sep: ", " (List.map (FirstSet.to_list s) ~f:(show_first_set_element))

module NonTerminal =
  struct
    type t = term
    let compare a b = match (a, b) with
      | (NonTerminal(a), NonTerminal(b)) -> String.compare a b
      | (Terminal(a), NonTerminal(b)) -> String.compare a b
      | (NonTerminal(a), Terminal(b)) -> String.compare a b
      | (Terminal(a), Terminal(b)) -> String.compare a b
    let sexp_of_t t = sexp_of_term t
    let t_of_sexp t = term_of_sexp t
  end

module NonTerminalMap = Map.Make(NonTerminal)

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

let show_follow_set s = String.concat ~sep: ", " (List.map (FollowSet.to_list s) ~f:(show_follow_set_element))

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
  | OR_EXPR of epsilonable_sequential_expr * or_expr
  | OR_EXPR_BASE of epsilonable_sequential_expr [@@deriving show]

and

epsilonable_sequential_expr =
  | Epsilon
  | SeqExpr of sequential_expr [@@deriving show]

and

sequential_expr =
  | SEQUENTIAL_EXPR of term * sequential_expr
  | SEQUENTIAL_EXPR_BASE of term [@@deriving show]
