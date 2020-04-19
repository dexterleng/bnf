open Core
open Types


let find_production (assignments: assignment list) (non_terminal: non_terminal) =
  List.find_exn assignments ~f:(fun assignment -> String.(=) assignment.lhs non_terminal)

let epsilon_only_set = FirstSet.add FirstSet.empty Epsilon

let rec generate_first_set_assignment (assignment: assignment) (first_set_map) =
  let rhs = assignment.rhs in
  let lhs = assignment.lhs in
  let (new_map, _) = generate_first_set_or_expr lhs rhs first_set_map in
  new_map
and

generate_first_set_expr (lhs: non_terminal) (expr: expr) (first_set_map: FirstSet.t NonTerminalMap.t) =
  generate_first_set_or_expr lhs expr first_set_map

and

generate_first_set_or_expr (lhs: non_terminal) (or_expr: or_expr) (first_set_map: FirstSet.t NonTerminalMap.t) =
  match or_expr with
    | OR_EXPR(sequential_expr, or_expr) ->
    (* UNION! *)
      let (seq_first_set_map, _) = generate_first_set_seq_expr lhs sequential_expr first_set_map in
      generate_first_set_or_expr lhs or_expr seq_first_set_map
    | OR_EXPR_BASE(sequential_expr) ->
      generate_first_set_seq_expr lhs sequential_expr first_set_map

and

generate_first_set_seq_expr (lhs: non_terminal) (seq_expr: sequential_expr) (first_set_map: FirstSet.t NonTerminalMap.t) =
  match seq_expr with
    | SEQUENTIAL_EXPR(primary_expr, sequential_expr) ->
      (* UNION until non-epsilonable *)
      let (primary_first_set_map, epsilonable) = generate_first_set_primary_expr lhs primary_expr first_set_map in
      if epsilonable then
        generate_first_set_seq_expr lhs sequential_expr primary_first_set_map
      else
        (primary_first_set_map, false)
    | SEQUENTIAL_EXPR_BASE(primary_expr) ->
      generate_first_set_primary_expr lhs primary_expr first_set_map

and


generate_first_set_primary_expr (lhs: non_terminal) (primary_expr: primary_expr) (first_set_map: FirstSet.t NonTerminalMap.t) =
  match primary_expr with
    | PRIMARY_EXPR(term) -> generate_first_set_term lhs term first_set_map
    | PRIMARY_PARENTHESIZED_EXPR(expr) -> generate_first_set_expr lhs expr first_set_map

and

generate_first_set_term (lhs: non_terminal) (term: term) (first_set_map: FirstSet.t NonTerminalMap.t) = match term with
  | NonTerminal(non_terminal) ->
    let non_terminal_first_set = NonTerminalMap.find_exn first_set_map (NonTerminal non_terminal) in
    let lhs_first_set = NonTerminalMap.find_exn first_set_map (NonTerminal lhs) in
    let new_lhs_first_set = FirstSet.union non_terminal_first_set lhs_first_set in
    let new_map = NonTerminalMap.set first_set_map ~key:(NonTerminal lhs) ~data:new_lhs_first_set in
    let epsilonable = FirstSet.mem non_terminal_first_set Epsilon in
    (new_map, epsilonable)
  | Terminal(terminal) ->
    let terminal_first_set = FirstSet.add (FirstSet.empty) (Terminal terminal) in
    let lhs_first_set = NonTerminalMap.find_exn first_set_map (NonTerminal lhs) in
    let new_lhs_first_set = FirstSet.union terminal_first_set lhs_first_set in
    let new_map = NonTerminalMap.set first_set_map ~key:(NonTerminal lhs) ~data:new_lhs_first_set in
    (new_map, false)
  | Epsilon ->
    let lhs_first_set = NonTerminalMap.find_exn first_set_map (NonTerminal lhs) in
    let new_lhs_first_set = FirstSet.add lhs_first_set Epsilon in
    let new_map = NonTerminalMap.set first_set_map ~key:(NonTerminal lhs) ~data:new_lhs_first_set in
    (new_map, true)

let generate_first_sets (assignments: assignment list) =
  let init_map = List.fold_left assignments
    ~f:(fun map assignment -> NonTerminalMap.add_exn map ~key: (NonTerminal assignment.lhs) ~data: FirstSet.empty)
    ~init: NonTerminalMap.empty
  in
  let map = ref init_map in
  let changed = ref true in
  while !changed do
    let new_map = List.fold_left assignments ~init: !map ~f:(fun map assignment -> generate_first_set_assignment assignment map) in
    changed := not (NonTerminalMap.equal (FirstSet.equal) !map new_map);
    map := new_map;
  done;
  !map
