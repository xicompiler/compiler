.PHONY: test

build:
	dune build

build-interpreter:
	cd interpreter && ./interpreter_build

test: build
	dune exec ./test/main.exe

test-cli: xic
	./test/cli/irrun_tests

xic:
	./xic-build

clean: bisect-clean
	dune clean
	find . -name '*.lexed' -delete
	find . -name '*.parsed' -delete
	find . -name '*.typed' -delete
	find . -name '*.output' -delete
	find . -name '*.conflicts' -delete
	rm -f xic *.zip *.log

cloc: clean
	cloc --by-file --include-lang=OCaml . --exclude-dir=uutf-library

bisect: bisect-clean
	dune exec --instrument-with bisect_ppx --force ./test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip: clean
	zip bfs45_dc854_vmj5_zak33.zip -r . -x@exclude.lst