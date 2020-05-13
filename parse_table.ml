open Core
open Types
open First

module ParseTableKey = struct
  (* by mere coincidence follow_set_element's type matches what we want here. *)
  type t = { lhs: non_terminal; terminal_or_end_symbol: follow_set_element } [@@deriving show, sexp]

  let compare a b =
    let a_lhs = a.lhs in
    let a_terminal = a.terminal_or_end_symbol in

    let b_lhs = b.lhs in
    let b_terminal = b.terminal_or_end_symbol in

    let compare_lhs = String.compare a_lhs b_lhs in
    let compare_terminal = FollowSetElement.compare a_terminal b_terminal in

    if compare_lhs = 0 then compare_terminal else compare_lhs
end

module ParseTableMap = Map.Make(ParseTableKey)

let rec or_expr_to_eps_seq_expr_list (or_expr: or_expr) = match or_expr with
  | OR_EXPR(eps_seq_expr, or_expr) -> eps_seq_expr::(or_expr_to_eps_seq_expr_list or_expr)
  | OR_EXPR_BASE(seq_expr) -> [seq_expr]

let generate_parse_table
  (assignments: assignment list)
  (first_set_map: first_set_of_assignment NonTerminalMap.t)
  (follow_set_map: FollowSet.t NonTerminalMap.t) =

  let init_parse_table = ParseTableMap.empty in

  List.fold_left assignments ~init: init_parse_table ~f:(fun parse_table assignment ->
    let lhs = assignment.lhs in  
    let rhs = assignment.rhs in

    let eps_seq_exprs = or_expr_to_eps_seq_expr_list rhs in
    let seq_expr_first_sets = (NonTerminalMap.find_exn first_set_map (NonTerminal lhs)).first_sets in

    (* asserting eql length as a sanity check *)
    let zipped_seq_expr_with_first_sets = List.zip_exn eps_seq_exprs seq_expr_first_sets in

    List.fold_left zipped_seq_expr_with_first_sets ~init: parse_table ~f:(fun parse_table (eps_seq_expr, seq_expr_first_set) ->
      (* for each terminal t in FIRST(seq), add lhs -> seq to table[lhs, t] *)
      let parse_table = FirstSet.fold seq_expr_first_set ~init: parse_table ~f:(fun parse_table first_set_element -> match first_set_element with
        | Terminal terminal -> ParseTableMap.add_exn parse_table ~key: ({ lhs = lhs; terminal_or_end_symbol = Terminal terminal; }) ~data: eps_seq_expr
        | Epsilon -> parse_table
      ) in

      (* 
        if epsilon is in first(seq), then for each terminal/end symbol t/e in follow(lhs),
        add lhs -> seq to table[lhs, t/e]
       *)
      let does_first_set_contain_epsilon = FirstSet.mem seq_expr_first_set Epsilon in
      let parse_table = if does_first_set_contain_epsilon
        then
          let follow_of_lhs = NonTerminalMap.find_exn follow_set_map (NonTerminal lhs) in

          FollowSet.fold follow_of_lhs ~init: parse_table ~f:(fun parse_table follow_set_element -> match follow_set_element with
            | Terminal terminal -> ParseTableMap.add_exn parse_table ~key: ({ lhs = lhs; terminal_or_end_symbol = Terminal terminal; }) ~data: eps_seq_expr
            | EndSymbol -> ParseTableMap.add_exn parse_table ~key: ({ lhs = lhs; terminal_or_end_symbol = EndSymbol; }) ~data: eps_seq_expr
          )
        else parse_table
      in
      
      parse_table
    )
  )


