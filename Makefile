.PHONY : all
all :
	dune build src/override.cmxa

.PHONY : clean
clean :
	dune clean

.PHONY : tests
tests :
	dune build tests/tests.exe
	_build/default/tests/tests.exe

.PHONY : install
install :
	dune build @install
	dune install
