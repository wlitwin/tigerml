all:
	ocamlbuild -I src -use-ocamlfind -pkg extlib -cflags -g -use-menhir -yaccflags --dump,--explain main.native

#-yaccflags --dump,--explain,--trace 

clean:
	ocamlbuild -clean
