open Core

let _ =
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let syntax = Parser.syntax Lexer.token lexbuf in
    let assignments = List.filter_map ~f:(fun x -> x) syntax in
    let t_prods = Follow.find_productions_by_non_terminal assignments "T" in
    List.iter t_prods ~f:(Fn.compose print_endline Types.show_assignment);
    Out_channel.flush stdout;