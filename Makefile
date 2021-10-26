hpack:
	hpack .

build: hpack
	cabal build

.PHONY: hpack build
