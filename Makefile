all:
	elm-make src/Scorekeeper.elm --yes --output scorekeeper-engine.js
	mv scorekeeper-engine.js assets/js/

clean:
	rm assets/js/scorekeeper-engine.js
	rm -r elm-stuff
