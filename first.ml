open Core
open Types

type first_set_of_assignment = {
  first_sets: FirstSet.t list;
  union_of_first_sets: FirstSet.t;
}

let find_production (assignments: assignment list) (non_terminal: non_terminal) =
  List.find_exn assignments ~f:(fun assignment -> String.(=) assignment.lhs non_terminal)

let epsilon_only_set = FirstSet.add FirstSet.empty Epsilon

let rec 

generate_first_sets (assignments: assignment list) =
  let init_map = List.fold_left assignments
    ~f:(fun map assignment -> NonTerminalMap.add_exn map
      ~key: (NonTerminal assignment.lhs)
      ~data: { first_sets = []; union_of_first_sets = FirstSet.empty }
    )
    ~init: NonTerminalMap.empty
  in
  let map = ref init_map in
  let changed = ref true in
  while !changed do
    let new_map = List.fold_left assignments ~init: !map ~f:(fun map assignment -> generate_first_set_assignment assignment map) in
    changed := not (NonTerminalMap.equal (fun a b -> FirstSet.equal a.union_of_first_sets b.union_of_first_sets) !map new_map);
    map := new_map;
  done;
  !map

and

generate_first_set_assignment (assignment: assignment) (first_set_map: first_set_of_assignment NonTerminalMap.t) =
  let lhs = assignment.lhs in
  let rhs = assignment.rhs in
  let first_set_of_assignment = generate_first_set_or_expr rhs [] first_set_map in
  NonTerminalMap.set first_set_map ~key: (NonTerminal lhs) ~data: first_set_of_assignment

and

generate_first_set_or_expr (or_expr: or_expr) (seq_expr_first_sets_rev: FirstSet.t list) (first_set_map: first_set_of_assignment NonTerminalMap.t) =
  match or_expr with
    | OR_EXPR(sequential_expr, or_expr) ->
      let seq_first_set = generate_first_set_seq_expr sequential_expr first_set_map in
      generate_first_set_or_expr or_expr (seq_first_set::seq_expr_first_sets_rev) first_set_map
    | OR_EXPR_BASE(sequential_expr) ->
      let seq_first_set = generate_first_set_seq_expr sequential_expr first_set_map in
      let seq_expr_first_sets = List.rev (seq_first_set::seq_expr_first_sets_rev) in
      let unions_of_first_sets = FirstSet.union_list seq_expr_first_sets in
      { first_sets = seq_expr_first_sets; union_of_first_sets = unions_of_first_sets }

and

generate_first_set_seq_expr (seq_expr: sequential_expr) (first_set_map: first_set_of_assignment NonTerminalMap.t) =
  match seq_expr with
    | SEQUENTIAL_EXPR(term, sequential_expr) ->
      (* UNION until non-epsilonable *)
      let term_first_set = generate_first_set_term term first_set_map in
      let does_first_set_contain_epsilon = FirstSet.mem term_first_set Epsilon in

      if does_first_set_contain_epsilon then
        FirstSet.union
          term_first_set
          (generate_first_set_seq_expr sequential_expr first_set_map)
      else
        term_first_set
    | SEQUENTIAL_EXPR_BASE(term) ->
      let term_first_set = generate_first_set_term term first_set_map in
      term_first_set

and

generate_first_set_term (term: term) (first_set_map: first_set_of_assignment NonTerminalMap.t) = match term with
  | NonTerminal(non_terminal) ->
    let non_terminal_first_set_of_assignment = NonTerminalMap.find_exn first_set_map (NonTerminal non_terminal) in
    non_terminal_first_set_of_assignment.union_of_first_sets
  | Terminal(terminal) ->
    let terminal_first_set = FirstSet.add (FirstSet.empty) (Terminal terminal) in
    terminal_first_set
  | Epsilon ->
    epsilon_only_set
