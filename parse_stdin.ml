open Core

let _ =
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let syntax = Parser.syntax Lexer.token lexbuf in
    let assignments = List.filter_map ~f:(fun x -> x) syntax in
    let one = List.nth_exn assignments 3 in
    let first_set = First.generate_first_set_assignment assignments one in
    print_endline (Types.show_first_set first_set);
    Out_channel.flush stdout;