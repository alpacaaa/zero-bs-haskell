
run-tests:
	cd test && ./run-tests

bundle-frontend:
	cd test && ./bundle-frontend && python -m SimpleHTTPServer 8800

generate-docs:
	# stack exec -- haddock --html --hyperlinked-source src/Zero/Server.hs --odir docs
	# cabal new-build --enable-documentation --haddock-for-hackage
	cabal new-haddock

build-pedantic:
	stack build --fast --pedantic
