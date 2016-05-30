# {{{ General settings
all: main

SRCDIR=src

BUILDDIR=localbuild
HIDIR=$(BUILDDIR)/hi
ODIR=$(BUILDDIR)/o
EXECUTABLE=sherlock

WINBUILDDIR=localbuild/win
WINHIDIR=$(WINBUILDDIR)/hi
WINODIR=$(WINBUILDDIR)/o
WINEXECUTABLE=sherlock.exe

MAIN=Main
# }}}

# {{{ Localbuild directories
$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(HIDIR): $(BUILDDIR)
	mkdir -p $(HIDIR)

$(ODIR): $(BUILDDIR)
	mkdir -p $(ODIR)


$(WINBUILDDIR):
	mkdir -p $(WINBUILDDIR)

$(WINHIDIR): $(WINBUILDDIR)
	mkdir -p $(WINHIDIR)

$(WINODIR): $(WINBUILDDIR)
	mkdir -p $(WINODIR)
# }}}

# {{{ make
wine: $(WINODIR) $(WINHIDIR)
	WINEDEBUG=-all wine ghc --make src/Main.hs -odir  $(WINODIR)   \
	                                           -hidir $(WINHIDIR) \
	                                           -isrc/
	mv src/$(MAIN).exe $(WINEXECUTABLE)

main: $(ODIR) $(HIDIR)
	ghc --make src/Main.hs -odir $(ODIR)   \
	                       -hidir $(HIDIR) \
	                       -isrc/
	mv src/$(MAIN) $(EXECUTABLE)

clean:
	rm -r $(BUILDDIR)
# }}}
