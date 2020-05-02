open Core

let _ =
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let syntax = Parser.syntax Lexer.token lexbuf in
    let assignments = List.filter_map ~f:(fun x -> x) syntax in
    let first_set_map = First.generate_first_sets assignments in
    let li = Types.NonTerminalMap.to_alist first_set_map in
    List.iter li ~f:(fun (term, first_set_of_assignment) ->
        Printf.printf "Term: %s, FirstSet: %s\n" (Types.show_term term) (Types.show_first_set first_set_of_assignment.union_of_first_sets);
    );

    Printf.printf("\n");

    let follow_set_map = Follow.generate_follow_sets assignments first_set_map in
    let li2 = Types.NonTerminalMap.to_alist follow_set_map in
    List.iter li2 ~f:(fun (term, follow_set) ->
        Printf.printf "Term: %s, FollowSet: %s\n" (Types.show_term term) (Types.show_follow_set follow_set);
    );

    let parse_table = Parse_table.generate_parse_table assignments first_set_map follow_set_map in

    let li3 = Parse_table.ParseTableMap.to_alist parse_table in
        List.iter li3 ~f:(fun (key, seq_expr) ->
        Printf.printf "Key: %s, SeqExpr: %s\n" (Parse_table.ParseTableKey.show key) (Types.show_sequential_expr seq_expr);
    );


    Out_channel.flush stdout;