.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

relative_path = _build/default/bin
absolute_path=$$(cd $$(dirname $(relative_path)) && pwd)/$$(basename $(relative_path))

mit:
	dune build
	mv $(relative_path)/main.exe $(relative_path)/mit
	@if [ "$(INITIALIZING_PATH)" = "true" ] && ! echo "$$PATH" | grep -qxF "${absolute_path}"; then \
		echo "export PATH=${absolute_path}:$$PATH" >> ~/.zshrc; \
		echo "export PATH=${absolute_path}:$$PATH" >> ~/.bashrc; \
	fi
	

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f finalproject.zip
	zip -r finalproject.zip . -x@exclude.lst

clean: bisect-clean
	dune clean
	rm -f finalproject.zip
