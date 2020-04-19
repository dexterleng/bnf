open Core

let _ =
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let syntax = Parser.syntax Lexer.token lexbuf in
    let assignments = List.filter_map ~f:(fun x -> x) syntax in
    let map = First.generate_first_sets assignments in
    let li = Types.NonTerminalMap.to_alist map in
    List.iter li ~f:(fun (term, first_set) ->
        Printf.printf "Term: %s, FirstSet: %s\n" (Types.show_term term) (Types.show_first_set first_set);
    );
    Out_channel.flush stdout;