all: build run

build:
	dune build

run:
	dune exec _build/default/bin/main.exe
