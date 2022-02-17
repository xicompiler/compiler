.PHONY: test

build:
	dune build

test: build
	dune exec ./test/main.exe

clean: bisect-clean
	dune clean
	find . -name '*.lexed'  -delete
	find . -name '*.parsed' -delete
	find . -name '*.output' -delete
	rm -f xic *.zip *.log

bisect: bisect-clean
	dune exec --instrument-with bisect_ppx --force ./test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip: clean
	zip zak33.zip -r . -x@exclude.lst