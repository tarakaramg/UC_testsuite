MAIN=dsl_test

OBJS =   test_types.cmo test_parser.cmo test_lexer.cmo     test_main.cmo 

%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<


$(MAIN): clean $(OBJS)
	ocamlc -g -o $(MAIN) unix.cma str.cma $(OBJS) 

test_lexer.ml : test_lexer.mll
	ocamllex -q $<

test_lexer.cmo : test_parser.cmi test_lexer.ml
	ocamlc -g -c test_lexer.ml

test_parser.cmo : test_parser.cmi 
	ocamlc -g -c test_parser.ml

test_parser.ml : test_parser.mly
	menhir --fixed-exception $<

test_parser.mli : test_parser.mly
	menhir --fixed-exception $<

clean:
	rm -f *.cmo *.cmi test_lexer.ml test_parser.ml test_parser.mli $(MAIN)
$(shell):
utop:
	$ utop  -init unix.cma str.cma test_parser.cmo test_lexer.cmo test_main.cmo $(shell)

