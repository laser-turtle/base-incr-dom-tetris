all:
	dune build bin/main.bc.js

show:
	firefox index.html

clean:
	dune clean
