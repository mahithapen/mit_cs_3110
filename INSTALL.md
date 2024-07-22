# Installation
1. Clone the repo from [GitHub](https://github.coecis.cornell.edu/nm549/cs3110_final_project).
2. Open the repo locally.
3. Open a command prompt in the project directory.
4. Run `make check`. This ensures you have the minimum OPAM, OCaml, and OUnit version requirements.
5. Run `make test`. This runs all unit tests. All tests should pass.
6. Run `make mit INITIALIZING_PATH=true`. This builds dune and exports the executable's path to the PATH in zsh. For any subsequent build on your machine, only run `make mit`.
7. Restart your terminal or run `source ~/.zshrc`. From any directory, you should be able to call `mit` or `mit help` to view the list of commands. Each command has a `--help` flag which displays additional inforamtion. For examples, see the Examples section in the README.
8. To remove the program from your machine, run `make clean` from the project directory.
