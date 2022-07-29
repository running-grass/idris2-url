.PHONY: help
help:
	echo "help me"

.PHONY: test
test: 
	pack run test/test.ipkg

.PHONY: test-parser
test-parser: 
	pack exec test/src/Test/Parser.idr

.PHONY: build
build: 
	pack build url.ipkg