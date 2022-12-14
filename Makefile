GHCFLAGS=-Wall -Wno-unused-matches -Wno-unused-local-binds -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults
GHCOBJFLAGS=--make -odir .objects -hidir .objects

TOOL_HS=$(wildcard tools/*.hs)
TOOLS_HHS=$(notdir $(TOOL_HS))
TOOLS_BINS=$(addprefix bin/,$(TOOLS_HHS))
STATIC_TOOLS_BINS=$(addprefix static-bin/,$(TOOLS_HHS))
TOOLS=$(TOOLS_BINS:.hs=)
STATIC_TOOLS=$(STATIC_TOOLS_BINS:.hs=)

all: tools

bin:
	mkdir -p bin
static-bin:
	mkdir -p static-bin

bin/%: tools/%.hs *.hs
	ghc $< $(GHCOBJFLAGS) -o $@ $(GHCFLAGS)
tools: bin $(TOOLS)

static-bin/%: tools/%.hs *.hs
	ghc $< -static $(GHCOBJFLAGS) -o $@ $(GHCFLAGS)
static: static-bin $(STATIC_TOOLS)

.PHONY: clean tools static
clean:
	rm -f .objects/* lam $(TOOLS) $(STATIC_TOOLS)
