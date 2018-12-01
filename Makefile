.PHONY: all clean byte native profile debug test

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

dev-server:
	$(MAKE) byte
	ocamlrun ./_build/src/main.byte -verbose

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte

profile:
	$(OCB) -tag profile main.native

debug:
	$(OCB) -tag debug main.byte

test: native
	./main.native "OCaml" "OCamlBuild" "users"

install-dep:
	opam install core core_kernel lwt ppx_deriving_yojson yojson cohttp
