.PHONY: help
help:
	echo "help me"

.PHONY: test
test: 
	pack run test/test.ipkg

.PHONY: test-http
test-http: 
	pack exec test/src/Test/HTTP.idr

.PHONY: build
build: 
	pack build url.ipkg