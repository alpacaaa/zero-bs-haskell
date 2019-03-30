
run-tests:
	cd test && ./run-tests

bundle-frontend:
	cd test \
	&& ./bundle-frontend \
	&& cp logo-wide.png dist \
	&& cd dist \
	&& python -m SimpleHTTPServer 8800

generate-docs:
	stack haddock