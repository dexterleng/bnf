open Core
open Types

let _ =
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let assignments = Parser.main Lexer.token lexbuf in
    let _ = List.iter assignments ~f:(fun assignment ->
      match assignment with
        | Nothing -> ()
        | Assignment(lhs, rhs) ->
          let lhs_str = match lhs with NonTerminal(lhs_nt) -> lhs_nt in
          let rhs_str = List.map rhs ~f:(fun t -> match t with
            | Left(NonTerminal(nt)) -> Printf.sprintf "<%s>" nt
            | Right(Terminal(t)) -> t
          )
          in
          Printf.fprintf stdout "<%s> ::= %s\n" lhs_str (String.concat ~sep:" " rhs_str);
          ()
    )
    in
    Out_channel.flush stdout;