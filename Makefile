GHCFLAGS=-Wall -Wno-unused-matches -Wno-unused-local-binds -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults
GHCOBJFLAGS=--make -odir .objects -hidir .objects

TOOL_HS=$(wildcard tools/*.hs)
TOOLS=$(TOOL_HS:.hs=)

lam: *.hs
	mkdir -p .objects
	ghc Lam.hs $(GHCOBJFLAGS) -o $@ $(GHCFLAGS)

tools/%: tools/%.hs *.hs
	ghc $< $(GHCOBJFLAGS) -o $@ $(GHCFLAGS)

tools: $(TOOLS)

all: lam

.PHONY: clean tools
clean:
	rm -f .objects/* lam $(TOOLS)
