#!/usr/bin/env bash

        ocamllex lexer.mll       # generates lexer.ml
        ocamlyacc parser.mly     # generates parser.ml and parser.mli
#		dune exec ./calc.exe
