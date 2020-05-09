open Core
open Types

type hello =
  | Terminal of terminal
  | NonTerminal of non_terminal
  | EndSymbol [@@deriving show, sexp]

type ast_node =
    NonTerminalNode of { non_terminal: non_terminal; children: ast_node list; }
    | TerminalNode of terminal
    | Leaf [@@deriving show, sexp]

exception ParseError of string

let rec seq_expr_to_term_list seq_expr = match seq_expr with
    | SEQUENTIAL_EXPR_BASE(term) -> [term]
    | SEQUENTIAL_EXPR(term, seq_expr) -> term::seq_expr_to_term_list(seq_expr)

let rec parse (parse_table: Types.sequential_expr Parse_table.ParseTableMap.t) (tokens: string list) (symbol: hello) =
    match symbol with
        | EndSymbol -> (Leaf, tokens)
        | Terminal(terminal) ->
            if terminal = List.hd_exn tokens then
                ((TerminalNode terminal), (List.tl_exn tokens))
            else
                raise (ParseError "dummy")
        | NonTerminal(non_terminal) ->
            if non_terminal = "GRAMMAR_PRIME" then (Leaf, tokens)
            else

            let first_token = List.hd_exn tokens in
            let seq_expr = Parse_table.ParseTableMap.find_exn parse_table { lhs = non_terminal; terminal_or_end_symbol = Terminal first_token; } in

            print_endline non_terminal;
            print_endline first_token;

            let (children_rev, tokens) = List.fold (seq_expr_to_term_list seq_expr) ~init:([], tokens) ~f:(fun (children_rev, tokens) term -> 
                match term with
                    | NonTerminal(non_terminal) ->
                        let (node, tokens) = parse parse_table tokens (NonTerminal non_terminal) in
                        (node::children_rev, tokens)
                    | Terminal(terminal) ->
                        let (node, tokens) = parse parse_table tokens (Terminal terminal) in
                        (node::children_rev, tokens)
                    | Epsilon ->
                        (children_rev, tokens)
                )
            in

            (NonTerminalNode { non_terminal = non_terminal; children = List.rev children_rev; }, tokens)

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

    let input_tokens = [
        "NON_TERMINAL"; "ASSIGNMENT_SYMBOL"; "NON_TERMINAL"; "TERMINAL"; "OR"; "TERMINAL"; "NEWLINE";
    ]
    in

    let (ast, _) = parse parse_table input_tokens (NonTerminal "GRAMMAR") in

    print_endline (show_ast_node ast);

    Out_channel.flush stdout;