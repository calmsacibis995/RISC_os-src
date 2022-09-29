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

SHELL=/bin/sh
REL=2.11
SRCROOT=
DESTROOT=
PCBOMDIR=$(SRCROOT)/usr/pkg/lib/pascal$(REL)/boms
PCBOMS=$(PCBOMDIR)/pc.lnk $(PCBOMDIR)/pc.lib $(PCBOMDIR)/pc.bsdlnk
CCBOMDIR=$(SRCROOT)/usr/pkg/lib/cmplrs$(REL)/boms
CCBOMS=$(CCBOMDIR)/cc.bin $(CCBOMDIR)/cc.dir $(CCBOMDIR)/cc.hdr $(CCBOMDIR)/cc.bsdhdr $(CCBOMDIR)/cc.lib $(CCBOMDIR)/cc.bsddir $(CCBOMDIR)/cc.bsdlib

all:
	@echo "nothing to do"

install: 
	DIR=`pwd` ; cd $(DESTROOT)/ ; \
		sh $$DIR/link-boms.sh $(PCBOMS); \
		sh $$DIR/link-boms.sh $(CCBOMS)

clobber:
	@echo "nothing to do"

clean:
	@echo "nothing to do"
