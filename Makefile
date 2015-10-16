COMPILER = ocamlbuild
FLAGS = -use-menhir -use-ocamlfind -pkg core -tag thread

MAIN = main.native
TEST = test.native

all: clean_natives $(MAIN) $(TEST)

$(MAIN): 
	$(COMPILER) $(FLAGS) $@

$(TEST):
	$(COMPILER) $(FLAGS) $@

clean: clean_natives
	rm -rf _build/

clean_natives:
	rm -f $(MAIN) $(TEST)
