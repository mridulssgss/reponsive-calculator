.ONESHELL:

init:
	mkdir .public

calc_build:
	elm make src/Main.elm --output=./.public/index.html
	cd ..

host:
	mv ./.public public

all:	init calc_build host

clean:
	rm -rf public

rebuild: clean all
