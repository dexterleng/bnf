open Core
open Types

type return_value = {
  follow_set_map: FollowSet.t NonTerminalMap.t;
  next_follow: FollowSet.t;
}

let follow_of_first (first_set: FirstSet.t) =
  FirstSet.fold first_set ~init: FollowSet.empty ~f:(fun follow_set first_set_element ->
    match first_set_element with
      | Terminal(terminal) -> FollowSet.add follow_set (Terminal(terminal))
      | Epsilon -> follow_set
  )

let first_of_follow (follow_set: FollowSet.t) =
  FollowSet.fold follow_set ~init: FirstSet.empty ~f:(fun first_set follow_set_element ->
    match follow_set_element with
      | Terminal(terminal) -> FirstSet.add first_set (Terminal(terminal))
      | EndSymbol -> first_set
  )

let rec

generate_follow_sets (assignments: assignment list) (first_set_map: FirstSet.t NonTerminalMap.t) =
  let init_map = List.fold_left assignments
    ~f:(fun map assignment -> NonTerminalMap.set map ~key: (NonTerminal assignment.lhs) ~data: FollowSet.empty)
    ~init: NonTerminalMap.empty
  in
  let follow_set_map = ref init_map in
  let changed = ref true in
  while !changed do
    let new_follow_set_map = List.fold_left assignments ~init: !follow_set_map ~f:(fun map assignment ->
      let result = generate_follow_set_assignment assignment first_set_map map in
      result.follow_set_map
    )
    in

    changed := not (NonTerminalMap.equal (FollowSet.equal) !follow_set_map new_follow_set_map);
    follow_set_map := new_follow_set_map;
  done;
  !follow_set_map

and

generate_follow_set_assignment (assignment: assignment) (first_set_map: FirstSet.t NonTerminalMap.t) (follow_set_map: FollowSet.t NonTerminalMap.t) =
  let follow_of_lhs = NonTerminalMap.find_exn follow_set_map (NonTerminal assignment.lhs) in
  generate_follow_set_or_expr assignment.rhs first_set_map follow_set_map follow_of_lhs

and

generate_follow_set_expr
  (expr: expr)
  (first_set_map: FirstSet.t NonTerminalMap.t)
  (follow_set_map: FollowSet.t NonTerminalMap.t)
  (next_follow: FollowSet.t)
  = generate_follow_set_or_expr expr first_set_map follow_set_map next_follow

and

generate_follow_set_or_expr
  (or_expr: or_expr)
  (first_set_map: FirstSet.t NonTerminalMap.t)
  (follow_set_map: FollowSet.t NonTerminalMap.t)
  (next_follow: FollowSet.t)
  = match or_expr with
    | OR_EXPR_BASE(seq_expr) ->
      generate_follow_set_seq_expr
        seq_expr first_set_map follow_set_map next_follow
    | OR_EXPR(seq_expr, or_expr) ->
      let result = generate_follow_set_or_expr
        or_expr first_set_map follow_set_map next_follow
      in
      generate_follow_set_seq_expr
        seq_expr first_set_map result.follow_set_map next_follow
and

generate_follow_set_seq_expr
  (seq_expr: sequential_expr)
  (first_set_map: FirstSet.t NonTerminalMap.t)
  (follow_set_map: FollowSet.t NonTerminalMap.t)
  (next_follow: FollowSet.t)
  = match seq_expr with
    | SEQUENTIAL_EXPR_BASE term ->
      generate_follow_set_term
        term first_set_map follow_set_map next_follow
    | SEQUENTIAL_EXPR(term, seq_expr) ->
      let result = generate_follow_set_seq_expr
        seq_expr first_set_map follow_set_map next_follow
      in
      generate_follow_set_term
        term first_set_map result.follow_set_map result.next_follow

and

generate_follow_set_term
  (term: term)
  (first_set_map: FirstSet.t NonTerminalMap.t)
  (follow_set_map: FollowSet.t NonTerminalMap.t)
  (next_follow: FollowSet.t)
  = match term with
    | Terminal(terminal) ->
      {
        follow_set_map;
        next_follow = FollowSet.of_list [Terminal(terminal)];
      }
    | NonTerminal(non_terminal) ->
      let new_follow_set_map =
        let follow_set = NonTerminalMap.find_exn follow_set_map (NonTerminal non_terminal) in
        let new_follow_set = FollowSet.union follow_set (next_follow) in
        NonTerminalMap.set follow_set_map ~key: (NonTerminal non_terminal) ~data: new_follow_set
      in

      let first_set = NonTerminalMap.find_exn first_set_map (NonTerminal non_terminal) in
      let does_first_set_contain_epsilon = FirstSet.mem first_set Epsilon in

      let new_next_follow = if does_first_set_contain_epsilon
        then FollowSet.union next_follow (follow_of_first first_set)
        else follow_of_first first_set
      in
      {
        follow_set_map = new_follow_set_map;
        next_follow = new_next_follow;
      }
    | Epsilon ->
      {
        follow_set_map;
        next_follow;
      }
      