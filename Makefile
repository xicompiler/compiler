build:
	dune build

lex:
	dune exec ./bin/main.exe

clean:
	dune clean
	rm -rf xic *.lexed *.zip

zip:
	make clean
	zip zak33.zip -r . -x@exclude.lst