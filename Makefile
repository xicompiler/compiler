build:
	dune build

lex:
	dune exec ./bin/main.exe

clean:
	dune clean
	rm -rf xic *.lexed
