GHCFLAGS=-Wall -Wno-unused-matches -Wno-unused-local-binds -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults
GHCOBJFLAGS=--make -odir .objects -hidir .objects

TOOL_HS=$(wildcard Tools/*.hs)
TOOLS=$(TOOL_HS:.hs=)

lam: *.hs
	mkdir -p .objects
	ghc Lam.hs $(GHCOBJFLAGS) -o $@ $(GHCFLAGS)

Tools/%: Tools/%.hs *.hs
	ghc $< $(GHCOBJFLAGS) -o $@ $(GHCFLAGS)

#Tools/NormalFormTool: *.hs Tools/NormalFormTool.hs
#	ghc Tools/NormalFormTool.hs $(GHCOBJFLAGS) -o $@ $(GHCFLAGS)

#Tools/ReduceTool: *.hs Tools/ReduceTool.hs
#	ghc Tools/ReduceTool.hs --make -odir .objects -hidir .objects -o $@ $(GHCFLAGS)

#Tools/BohmTool: *.hs Tools/BohmTool.hs
#	ghc Tools/BohmTool.hs --make -odir .objects -hidir .objects -o $@ $(GHCFLAGS)

tools: $(TOOLS)
	echo $(TOOLS)
#foo := a.o b.o c.o
#bar := $(foo:.o=.c)

all: lam

.PHONY: clean tools
clean:
	rm -f .objects/* lam $(TOOLS)
