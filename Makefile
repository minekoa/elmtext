
all : compile

env:
	elm-package install

compile:
	elm-make Main.elm --output Main.js

