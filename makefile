all:
	ocamlbuild -cflag -g -lflag -g -I src -use-ocamlfind -pkg extlib -use-menhir main.byte

#-yaccflags --dump,--explain,--trace 

clean:
	ocamlbuild -clean
