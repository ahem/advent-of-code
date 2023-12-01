# Advent of Code 2023

## Setup

```command
opam switch create . 5.1.0 --deps-only
eval $(opam env)
opam install core re2 dune ocaml-lsp-server merlin ocamlformat utop
```

## Run code

```
cat 1/input.txt | dune exec 1/main.exe
```
