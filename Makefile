# {{{ General settings
all: main

SRCDIR=src/

BUILDDIR=localbuild
HIDIR=$(BUILDDIR)/hi
ODIR=$(BUILDDIR)/o

EXECUTABLE=gui_test.x
MAIN=Main
# }}}

# {{{ Localbuild directories
$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(HIDIR): $(BUILDDIR)
	mkdir -p $(HIDIR)

$(ODIR): $(BUILDDIR)
	mkdir -p $(ODIR)
# }}}

# {{{ make
main: $(ODIR) $(HIDIR)
	ghc --make src/Main.hs -odir $(ODIR) -hidir $(HIDIR) -isrc/
	mv src/$(MAIN) $(EXECUTABLE)

clean:
	rm -r $(BUILDDIR)
# }}}
