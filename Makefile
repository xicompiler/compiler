.PHONY:test

build:
	dune build

lex:
	dune exec ./bin/main.exe

test:
	dune exec ./tests_unit/main.exe

clean:
	dune clean
	rm -rf xic *.lexed *.zip

bisect: bisect-clean
	dune exec --instrument-with bisect_ppx --force ./tests_unit/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	make clean
	zip zak33.zip -r . -x@exclude.lst