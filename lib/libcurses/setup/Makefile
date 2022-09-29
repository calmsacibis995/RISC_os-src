#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.4.1.3 90/05/10 02:32:16 wje Exp $
#
#$Header: Makefile,v 1.4.1.3 90/05/10 02:32:16 wje Exp $
# cursor setup maker - this makefile must be run before libcurses and
# all of the libcurses tools (located in the "tic" directory in the
# command source tree)
#

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

BUILT	=term.h keycaps curses.h keyname.c termcap.c tiget.c tifget.c \
	tnames.c tinames.c tifnames.c

TMP	=./tmp

SRCS   =Makefile  \
	curses.ed maketerm.ed \
	termcap.ed tiget.ed tifget.ed \
	caps \
	setkeymap.c setupterm.c keyname.sh \
	unctrl.h curses_inc.h curshdr.h uparm.h copyright.h

HDRS	=

INSTHDR	= curses.h term.h unctrl.h

HDRWHEREIGO=$(DESTROOT)/usr/include
BSDHDR	= $(DESTROOT)/bsd43/usr/include

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)


SETUPTERM=$(BUILT)

all: $(SETUPTERM)
 
$(SETUPTERM):
term.h tnames.c tinames.c tifnames.c: caps maketerm.ed
	@if [ ! -d "$(TMP)" ] ;\
	then	echo "\t$(TMP) not found.";\
		echo "\n\tCreating $(TMP)\n";\
		mkdir $(TMP); \
	fi;
	@echo
	@echo Building term.h
	@echo
	ed - < maketerm.ed
	chmod 0664 term.h tnames.c tinames.c tifnames.c
	@echo
	@echo "term.h has been built"
	@echo

keycaps: caps
	@echo
	@echo Building keycaps
	@echo
	rm -rf $(TMP)/keycaps keycaps
	grep "KEY_" caps > keycaps
	@echo 
	@echo "keycaps has been built"
	@echo

curses.h: keycaps curses.ed
	@if [ ! -d "$(TMP)" ] ;\
	then	echo "\t$(TMP) not found.";\
		echo "\n\tCreating $(TMP)\n";\
		mkdir $(TMP); \
	fi;
	@echo 
	@echo Building ./curses.h
	@echo 
	rm -f curses.h
	ed - < curses.ed
	chmod 0664 curses.h
	rm -rf $(TMP)/keycaps
	@echo 
	@echo "curses.h has been built"
	@echo 

keyname.c: keycaps keyname.sh
	@echo
	@echo Building keyname.c
	@echo
	rm -f keyname.c
	sh keyname.sh
	chmod 0664 keyname.c
	@echo
	@echo "keyname.c has been built"
	@echo

setkeymap.c: keycaps
	@if tail -1 keycaps | grep 'KEY_F(63)' > /dev/null;then :; \
		else echo new keys!! must change setkeymap.c; exit 1;fi

termcap.c:	termcap.ed caps
	@if [ ! -d "$(TMP)" ] ;\
	then	echo "\t$(TMP) not found.";\
		echo "\n\tCreating $(TMP)\n";\
		mkdir $(TMP); \
	fi;
	@echo 
	@echo Building termcap.c
	@echo
	rm -f termcap.c
	ed - < termcap.ed
	rm -f bool num str
	chmod 0664 termcap.c
	@echo 
	@echo "termcap.c has been built"
	@echo

tifget.c:	tifget.ed caps
	@if [ ! -d "$(TMP)" ] ;\
	then	echo "\t$(TMP) not found.";\
		echo "\n\tCreating $(TMP)\n";\
		mkdir $(TMP); \
	fi;
	@echo
	@echo Building tifget.c
	@echo
	ed - < tifget.ed
	chmod 0664 tifget.c
	@echo
	@echo "tifget.c has been built"
	@echo

tiget.c:	tiget.ed caps
	@if [ ! -d "$(TMP)" ] ;\
	then	echo "\t$(TMP) not found.";\
		echo "\n\tCreating $(TMP)\n";\
		mkdir $(TMP); \
	fi;
	@echo
	@echo Building tiget.c
	@echo
	ed - < tiget.ed
	chmod 0664 tiget.c
	@echo
	@echo "tiget.c has been built"
	@echo

clean:
	rm -f *.o
	rm -rf $(BUILT)

clobber: clean
	rm -rf $(TMP)

generic:
	$(GENERIC) -s $(SRCS)

install:

install-headers: all
	$(INSTALL) $(INSTOPTS) curses.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) term.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/unctrl.h unctrl.h $(HDRWHEREIGO)
