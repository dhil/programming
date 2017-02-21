OCAMLBUILD := ocamlbuild -classic-display -use-menhir -cflags "-g" -lflags "-g"
INCLUDE    := -Is pllib
MAIN       := main
EXEC       := joujou
SUFFIX     := native

.PHONY: all clean test

all:
	$(OCAMLBUILD) $(INCLUDE) $(MAIN).$(SUFFIX)
	ln -sf $(MAIN).$(SUFFIX) $(EXEC)

clean:
	rm -f *~ $(EXEC)
	$(OCAMLBUILD) -clean

test: all
	$(MAKE) -C ../test $@
