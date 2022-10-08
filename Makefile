GHCFLAGS=-Wall -Wno-unused-matches -Wno-unused-local-binds -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults

lam: *.hs
	mkdir -p .objects
	ghc Lam.hs --make -odir .objects -hidir .objects -o $@ $(GHCFLAGS)

all: lam

.PHONY: clean
clean:
	rm -f .objects/* lam
