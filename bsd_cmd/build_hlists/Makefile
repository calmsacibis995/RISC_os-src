#
# $Header: Makefile,v 1.4.1.2 90/08/06 14:40:01 hawkes Exp $
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
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/bsd43/bin/

SRCS	=build_hlists.sh

WHOAMI	=build_hlists

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	$(WHEREIGO)/$(WHOAMI)	# run command to build necessary hash lists

$(WHOAMI): $(SRCS)
	-rm -f $(WHOAMI)
	cp $(SRCS) $(WHOAMI)

clean:

clobber:
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -p $(WHOAMI) \
		-R$(DESTROOT) -D$(WHEREIGO)
