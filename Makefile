.PHONY: test

build:
	dune build

build-interpreter:
	cd interpreter && ./interpreter_build

test:
	./xic-build
	dune exec ./test/main.exe
	

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

format:
	ocamlformat */*.ml */*.mli --inplace

format-check:
	ocamlformat */*.ml */*.mli --check

zip: clean
	zip bfs45_dc854_vmj5_zak33.zip -r . -x@exclude.lst