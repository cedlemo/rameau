.PHONY : dev_run
dev_run :
	opam reinstall libmpdclient
	jbuilder build src/rameau.exe && ./_build/default/src/rameau.exe

.PHONY : clean
clean :
	jbuilder clean
