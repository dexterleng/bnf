type token =
  | NON_TERMINAL of (string)
  | TERMINAL of (string)
  | OR
  | ASSIGN
  | NEWLINE

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  259 (* OR *);
  260 (* ASSIGN *);
  261 (* NEWLINE *);
    0|]

let yytransl_block = [|
  257 (* NON_TERMINAL *);
  258 (* TERMINAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\006\000\006\000\005\000\
\005\000\007\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\002\000\001\000\002\000\001\000\
\001\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\003\000\011\000\000\000\000\000\000\000\
\001\000\002\000\008\000\009\000\004\000\000\000\000\000\005\000\
\006\000\007\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\013\000\015\000\016\000\017\000"

let yysindex = "\002\000\
\255\254\000\000\001\255\000\000\000\000\255\254\002\255\000\255\
\000\000\000\000\000\000\000\000\000\000\000\255\000\255\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\255\002\255\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\003\000\000\000\000\000\000\000\254\255\249\255\000\000"

let yytablesize = 9
let yytable = "\003\000\
\011\000\012\000\001\000\004\000\008\000\014\000\010\000\018\000\
\009\000"

let yycheck = "\001\001\
\001\001\002\001\001\000\005\001\004\001\008\000\005\001\015\000\
\006\000"

let yynames_const = "\
  OR\000\
  ASSIGN\000\
  NEWLINE\000\
  "

let yynames_block = "\
  NON_TERMINAL\000\
  TERMINAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 11 "parser.mly"
            ( 1 )
# 79 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'assignment) in
    Obj.repr(
# 14 "parser.mly"
                     ( 1 )
# 86 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    Obj.repr(
# 15 "parser.mly"
            ( 1 )
# 92 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 19 "parser.mly"
                           ( 1 )
# 100 "parser.ml"
               : 'assignment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'either_terminal) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprP) in
    Obj.repr(
# 23 "parser.mly"
                        ( 1 )
# 108 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'epsilon) in
    Obj.repr(
# 27 "parser.mly"
          ( 1 )
# 115 "parser.ml"
               : 'exprP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'either_terminal) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprP) in
    Obj.repr(
# 28 "parser.mly"
                          ( 1 )
# 123 "parser.ml"
               : 'exprP))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "parser.mly"
               ( 1 )
# 130 "parser.ml"
               : 'either_terminal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
            ( 1 )
# 137 "parser.ml"
               : 'either_terminal))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
  ( 1 )
# 143 "parser.ml"
               : 'epsilon))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
