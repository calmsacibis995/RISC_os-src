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
# $Header: Makefile,v 1.4.1.2 90/05/09 14:42:16 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

DEBUG	=
MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(DEBUG) -systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =$(DEBUG) -systype bsd43 $(GNUM) $(EASFLAGS)
#CFLAGS	=$(DEBUG) -DKERNEL -DRISCOS $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
#ASFLAGS =$(DEBUG) -DKERNEL -DRISCOS $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/bsd43/usr/lib
HDR_WHEREIGO=$(DESTROOT)/usr/include/rpcsvc
BSDHDR=$(DESTROOT)/bsd43/usr/include/rpcsvc

OBJS	=bootparam_xdr.o etherxdr.o klm_prot.o mountxdr.o nlm_prot.o publickey.o \
	rex_xdr.o rquotaxdr.o \
	rstatxdr.o rusersxdr.o rwallxdr.o sm_inter.o sprayxdr.o util.o \
	xcrypt.o yppasswdxdr.o

SRCS	=bootparam_xdr.c etherxdr.c klm_prot.c mountxdr.c nlm_prot.c \
	publickey.c rex_xdr.c rquotaxdr.c \
	rstatxdr.c rusersxdr.c rwallxdr.c sm_inter.c sprayxdr.c util.c \
	xcrypt.c yppasswdxdr.c

HDRS	=bootparam.h ether.h klm_prot.h mount.h nlm_prot.h pwdnm.h \
	rex.h rquota.h rstat.h \
	rusers.h rwall.h sm_inter.h spray.h yppasswd.h

XFILES	= bootparam_prot.x ether.x klm_prot.x mount.x nfs_prot.x \
	nlm_prot.x rex.x rnusers.x rquota.x rstat.x sm_inter.x \
	spray.x yp.x yppasswd.x

LIBSUFFIX=
WHOAMI	=librpcsvc$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

install-headers: $(HDRS)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/bootparam.h bootparam.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/ether.h ether.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/klm_prot.h klm_prot.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/mount.h mount.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/nlm_prot.h nlm_prot.h $(HDR_WHEREIGO) 
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/pwdnm.h pwdnm.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/rex.h rex.h $(HDR_WHEREIGO) 
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rquota.h rquota.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rstat.h rstat.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rusers.h rusers.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rwall.h rwall.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/sm_inter.h sm_inter.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/spray.h spray.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/yppasswd.h yppasswd.h $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/bootparam_prot.x bootparam_prot.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/ether.x ether.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/klm_prot.x klm_prot.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/mount.x mount.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/nfs_prot.x nfs_prot.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/nlm_prot.x nlm_prot.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rex.x rex.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rnusers.x rnusers.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rquota.x rquota.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/rstat.x rstat.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/sm_inter.x sm_inter.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/spray.x spray.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/yp.x yp.x $(HDR_WHEREIGO)
	$(INSTALL) $(INSTOPTS)  -l $(BSDHDR)/yppasswd.x yppasswd.x $(HDR_WHEREIGO)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)
