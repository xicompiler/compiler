.PHONY: test

build:
	dune build

test:
	dune exec ./test/main.exe

parsing-test:
	dune exec ./test/ParsingTests.exe

clean:
	dune clean
	rm -rf xic *.lexed *.zip *.log

bisect: bisect-clean
	dune exec --instrument-with bisect_ppx --force ./test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	make clean
	zip zak33.zip -r . -x@exclude.lst