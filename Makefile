
OCB_FLAGS = -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)

all: native

native:
	$(OCB) main.native

clean:
	$(OCB) -clean
