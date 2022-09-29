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
# $Header: Makefile,v 1.2.1.3 90/05/10 05:39:24 wje Exp $
#$Header: Makefile,v 1.2.1.3 90/05/10 05:39:24 wje Exp $
DEPTH	=../..
LABEL	=klib_des
include $(DEPTH)/kcommondefs

HDRS	=des.h des_crypt.h softdes.h

SRCS	=des_crypt.c des_soft.c softdesdata.c

LIBNAME	=$(BOOTAREA)/libdes.a

default:	all

rules:	$(MAKERULES)

$(MAKERULES):	$(MAKERULESMARK)

$(MAKERULESMARK): Makefile
	touch $(MAKERULESMARK) ; \
	rm -rf $(MKRULESTMP) ; \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' OBJS $(SRCS) \
		> $(MKRULESTMP) && \
	mv $(MKRULESTMP) $(MAKERULES)

include $(MAKEDEPEND)

include $(MAKERULES)

LIBOBJS=$(OBJS)

all: $(LIBMARK)

source-links: link-mark

link-mark: Makefile
	for i in $(HDRS) $(SRCS) ;  \
	{ \
		rm -rf $$i ; \
		ln -s ../../../../bsd_lib/libc/des/$$i $$i ; \
	} ;
	touch link-mark

clean:
	-rm -f $(OBJS) $(HDRS) $(SRCS) link-mark

clobber: clean 

include $(KCOMMONRULES)
