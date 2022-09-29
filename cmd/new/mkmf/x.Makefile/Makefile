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
# $Header: Makefile,v 1.3.1.2 90/05/09 18:07:22 wje Exp $
#
SHELL         = /bin/sh
ETCDIR        = /etc

DESTROOT      =

INSTALL       = $(ETCDIR)/mipsinstall
MODE          = 444
OWNER         = bin
GROUP         = bin
ISTRIP        =
ICOPY         = -c
INSTOPTS      = $(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

DEST	      = $(DESTROOT)/usr/new/lib

MAKEFILE      = Makefile

PRINT	      = pr

PROGRAM	      = p.Makefile

SRCS	      = l.Makefile \
		p.Makefile \
		t.Makefile

SUFFIX	      = .Makefile:s

VERSION	      =

all:;

clobber:;

clean:;

co:;		@co -r$(VERSION) $(SRCS)

depend:;

diff:;		@rcsdiff -r$(VERSION) $(HDRS) $(SRCS)

index:;

install:	$(SRCS)
		for i in $(SRCS) ; do \
		$(INSTALL) $(INSTOPTS) $$i $(DEST) ; \
		done

lint:;

print:;		@$(PRINT) $(SRCS)

tags:;

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS)
		@make -f $(MAKEFILE) DEST=$(DEST) install tags
