open Core

let _ =
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let syntax = Parser.syntax Lexer.token lexbuf in
    print_endline (Types.show_syntax syntax);
    Out_channel.flush stdout;