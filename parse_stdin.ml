open Core

let _ =
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let syntax = Parser.syntax Lexer.token lexbuf in
    let assignments = List.filter_map ~f:(fun x -> x) syntax in
    let first_set_map = First.generate_first_sets assignments in
    let li = Types.NonTerminalMap.to_alist first_set_map in
    List.iter li ~f:(fun (term, first_set) ->
        Printf.printf "Term: %s, FirstSet: %s\n" (Types.show_term term) (Types.show_first_set first_set);
    );

    Printf.printf("\n");

    let follow_set_map = Follow.generate_follow_sets assignments first_set_map in
    let li2 = Types.NonTerminalMap.to_alist follow_set_map in
    List.iter li2 ~f:(fun (term, follow_set) ->
        Printf.printf "Term: %s, FollowSet: %s\n" (Types.show_term term) (Types.show_follow_set follow_set);
    );

    Out_channel.flush stdout;