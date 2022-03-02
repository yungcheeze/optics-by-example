hpack:
	hpack .

build: hpack
	cabal build

test: build
	cabal test --test-show-details=direct

.PHONY: hpack build
