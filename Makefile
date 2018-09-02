.PHONY : reinstall_libmpd
reinstall_libmpd:
	opam reinstall libmpdclient

.PHONY : dev_run
dev_run :
	dune build src/rameau.exe --profile release && ./_build/default/src/rameau.exe

.PHONY : run
run :
	dune build src/rameau.exe && ./_build/default/src/rameau.exe

.PHONY : clean
clean :
	dune clean
