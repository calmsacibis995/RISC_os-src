CPP = cc -E
MAKE = make  # BSD doesn't have it as a default.
#Note: an alternative is  CPP = /lib/cpp

all: xmakefile doall

doall:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} all

#This is used in making a distribution.
#Do not use it on development directories!
distclean:
	-rm -f paths.h config.h emacs-* temacs xemacs xmakefile core *~ \#* *.o

clean:
	-rm -f temacs xemacs xmakefile core \#* *.o

xemacs: xmakefile doxemacs

doxemacs:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} xemacs

temacs: xmakefile dotemacs

dotemacs:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} temacs

# If you have a problem with cc -E here, changing
# the definition of CPP above may fix it.
xmakefile: ymakefile config.h
	rm -f xmakefile
	cp ymakefile junk.c
	$(CPP) junk.c | sed -e 's/^#.*//' -e 's/^[ \f\t][ \f\t]*$$//' -e 's/^ /	/' | \
	sed -n -e '/^..*$$/p' > xmakefile
	rm -f junk.c

tags:
	etags [a-z]*.h [a-z]*.c ../lisp/[a-z]*.el
