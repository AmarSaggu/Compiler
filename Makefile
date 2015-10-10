COMPILER = ocamlbuild
FLAGS = -use-menhir -use-ocamlfind -pkg core -tag thread

MAIN = main.native

all: clean_main $(MAIN)

$(MAIN): 
	$(COMPILER) $(FLAGS) $@

clean: clean_main
	rm -rf _build/

clean_main:
	rm -f $(MAIN)
