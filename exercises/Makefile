
run-tests:
	cd test && ./run-tests

build-frontend:
	cd test \
	&& ./bundle-frontend \
	&& cp logo-wide.png dist \
	&& cp index.html dist

bundle-frontend:
	make build-frontend \
	&& cd test/dist \
	&& rm index.html \
	&& python -m SimpleHTTPServer 8800

generate-docs:
	stack haddock