GHCFLAGS	= -Wall -O2
GHC		= ghc $(GHCFLAGS) $(PKGFLAGS)

hxpath		= ./HXPath
xmltr		= ./xmltr

all		: $(hxpath) $(xmltr)

force		:
		$(GHC) --make -o $(prog) $(prog).hs

$(hxpath)	: $(hxpath).hs
		$(GHC) --make -o $@ $<

$(xmltr)	: $(xmltr).hs
		$(GHC) --make -o $@ $<

clean		:
		rm -f $(prog) *.hi *.o $(hxpath) $(xmltr)
