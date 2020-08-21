default: dsl-test

main: dsl-test.native

%.native: 
	ocamlbuild -use-ocamlfind $@
	mv $@ $*

.PHONY: default
