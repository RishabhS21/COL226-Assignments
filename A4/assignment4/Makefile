make:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c main.ml
	ocamlc -o ast lexer.cmo parser.cmo ast.cmo main.cmo
clean:
	rm ast *.cmi *.cmo *.mli lexer.ml parser.ml