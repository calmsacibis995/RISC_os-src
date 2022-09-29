# |-----------------------------------------------------------|
# | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
# 
# $Header: Makefile,v 1.3.1.2 90/01/11 17:55:50 wje Exp $ 

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

CROSSROOT=
EXPAND=$(CROSSROOT)/bsd43/bin/expand
UNEXPAND=$(CROSSROOT)/bsd43/bin/unexpand
GETNAME=$(CROSSROOT)/usr/lib/getNAME
MAKEWHATIS=$(CROSSROOT)/usr/lib/makewhatis

DEBUG	=
MAXOPT	=
INCLUDE	=
DEFS	=
DEF_CFLAGS=$(MAXOPT) -g3
CFLAGS	=-systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) $(DEFS) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)
LIBRARIES= -ltermcap
LOCAL_LIBS=

TMACDIR =/usr/lib/tmac
TMACFILE=$(TMACDIR)/tmac.an
DESTTMACFILE=$(DESTROOT)$(TMACFILE)

NROFF	=cat $(DESTTMACFILE) - | tbl | nroff | col
TROFF	=cat $(DESTTMACFILE) - | tbl | troff -Ti10 | lpr -n

WHEREIGO=$(DESTROOT)/usr/new
WHEREIGOMAN=$(DESTROOT)/usr/man
MANDIR=u_man/man1
DESTMANDIR=$(WHEREIGOMAN)/$(MANDIR)
DESTCATDIR=$(WHEREIGOMAN)/catman/$(MANDIR)

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)
BASICINSTOPTS=$(ICOPY) $(ISTRIP) -o $(OWNER) 
MANINSTOPTS=$(ICOPY) -m 444 -o $(OWNER) -g $(GROUP) -f

SRCS	=hash.c groupdir.c envir_set.c newsrc.c pagefile.c reader.c storage.c sig_set.c term_set.c tty_set.c userlist.c vn.c vnglob.c digest.c strings.c ucb.c reg.c

OBJS=$(SRCS:.c=.o)

HDRS	=config.h head.h reader.h tty.h tune.h vn.h

WHOAMI	=vn

all: $(WHOAMI)

install: all install-only

install-files: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

install-man:
	if [ -d $(DESTCATDIR)/. ] ; \
	then \
		rm -f nroff.tmp ;\
		cat vn.man | $(NROFF) > nroff.tmp ;\
		$(INSTALL) $(MANINSTOPTS) nroff.tmp $(DESTCATDIR)/vn.1 ;\
		rm -f nroff.tmp ;\
	fi
	if [ -d $(DESTMANDIR)/. ] ; \
	then \
	    if [ ! -d $(DESTCATDIR)/. ] || \
	       [ `(cd $(DESTCATDIR) ; /bin/pwd )` != `(cd $(DESTMANDIR)/. ; /bin/pwd )` ] ; \
	    then \
		$(INSTALL) $(MANINSTOPTS) vn.man $(DESTMANDIR)/vn.1 ;\
	    fi ; \
	fi

install-whatis: install-man
	$(MAKEWHATIS) -EXPAND $(EXPAND) -UNEXPAND $(UNEXPAND) \
		-GETNAME $(GETNAME) $(WHEREIGOMAN)/catman
	$(INSTALL) $(MANINSTOPTS) $(WHEREIGOMAN)/catman/whatis \
		$(WHEREIGOMAN)/whatis
	-rm -f $(WHEREIGOMAN)/catman/whatis

install-only: install-files install-whatis

$(OBJS): $(HDRS)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS) 
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

troff:
	cat vn.man | $(TROFF)

