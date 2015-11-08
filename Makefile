COMPILER = ocamlbuild
FLAGS = -use-menhir -use-ocamlfind -pkg core -tag thread

SRC=src

MAIN = main.native
TEST = test.native
BENCH = bench.native

all: clean_natives $(MAIN) $(TEST) $(BENCH)

$(MAIN): 
	$(COMPILER) $(FLAGS) $(SRC)/$@

$(TEST):
	$(COMPILER) $(FLAGS) $(SRC)/$@

$(BENCH):
	$(COMPILER) $(FLAGS) $(SRC)/$@

clean: clean_natives
	rm -rf _build/

clean_natives:
	rm -f $(MAIN) $(TEST) $(BENCH)
