
run-tests:
	./exercises/run-tests

bundle-frontend:
	./exercises/bundle-frontend
	cd exercises && python -m SimpleHTTPServer 8800
