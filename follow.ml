open Core
open Types

let rec does_assignment_have_non_terminal (assignment: assignment) (non_terminal: non_terminal) =
  does_expr_have_non_terminal assignment.rhs non_terminal

and

does_expr_have_non_terminal expr non_terminal =
  does_or_expr_have_non_terminal expr non_terminal

and

does_or_expr_have_non_terminal or_expr non_terminal = match or_expr with
  | OR_EXPR(seq_expr, or_expr) ->
    let found = does_seq_expr_have_non_terminal seq_expr non_terminal in
    if found then found
    else does_or_expr_have_non_terminal or_expr non_terminal
  | OR_EXPR_BASE(seq_expr) ->
    does_seq_expr_have_non_terminal seq_expr non_terminal

and

does_seq_expr_have_non_terminal seq_expr non_terminal = match seq_expr with
  | SEQUENTIAL_EXPR(primary_expr, seq_expr) ->
    let found = does_primary_expr_have_non_terminal primary_expr non_terminal in
    if found then found
    else does_seq_expr_have_non_terminal seq_expr non_terminal
  | SEQUENTIAL_EXPR_BASE(primary_expr) ->
    does_primary_expr_have_non_terminal primary_expr non_terminal

and

does_primary_expr_have_non_terminal primary_expr non_terminal = match primary_expr with
  | PRIMARY_EXPR(term) -> (match term with
      | NonTerminal(matching_non_terminal) -> String.(=) matching_non_terminal non_terminal
      | _ -> false
    )
  | PRIMARY_PARENTHESIZED_EXPR(expr) -> does_expr_have_non_terminal expr non_terminal

let find_productions_by_non_terminal (assignments: assignment list) (non_terminal: non_terminal) =
  List.filter assignments ~f:(fun a -> does_assignment_have_non_terminal a non_terminal)

(* let generate_follow_set assignments non_terminal =
  let productions_with_non_terminal = find_productions_by_non_terminal assignments non_terminal in
  List.fold_left productions_with_non_terminal FollowSet.empty ~f:(fun set production ->
    let first_of_user = First.generate_first_set_assignment assignments production in
  ) *)
