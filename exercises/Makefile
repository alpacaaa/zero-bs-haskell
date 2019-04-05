
run-tests:
	cd test && ./run-tests

build-frontend:
	cd test \
	&& ./bundle-frontend \
	&& cp logo-wide.png dist \
	&& cp index.html dist

liveserver:
	make build-frontend \
	&& cd test/dist \
	&& rm index.html \
	&& python -m SimpleHTTPServer 8800

generate-docs:
	cd ../library \
	&& stack haddock

deploy:
	make build-frontend \
	&& make generate-docs \
	&& cd ../library \
	&& cp -R `stack path --local-doc-root`/zero-bullshit-0.1.0.0 ../exercises/test/dist/docs \
	&& cd .. \
	&& cp -R exercises/test/dist/ alpacaaa.github.io/zero-bullshit-haskell/ \
	&& cd alpacaaa.github.io \
	&& git add zero-bullshit-haskell \
	&& git commit -m "Update website"