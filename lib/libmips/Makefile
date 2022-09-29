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
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.2.2.3.1.2 90/07/19 13:01:18 hawkes Exp $
#
SHELL=	/bin/sh
ETCDIR=	/etc


DESTROOT=

SYSTYPE=
MAXOPT= -O2
INCLUDE=
CFLAGS= $(SYSTYPE) $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS= $(SYSTYPE) $(GNUM) $(EASFLAGS)
LFLAGS= $(ELFLAGS)
YFLAGS= $(EYFLAGS)

AR= ar
ARFLAGS= crs

WHEREIGO=.

SRCS= libutil.c machine_setup.c kmem.c
HDRS= libutil.h machine_setup.h
OBJS	=$(SRCS:.c=.o)

VERSIONDIRS= bsd43 sysv
VERSIONS= libmips_bsd43.a libmips_sysv.a

WHOAMI= $(VERSIONS)

default: all

$(VERSIONDIRS):
	[ -d $@ ] || mkdir $@

$(VERSIONS): $(VERSIONDIRS)
	X=`expr $@ : "libmips_\(.*\).a" ` ; \
	$(MAKE) $(MAKEARGS) libmips_X X=$$X

libmips_X: $(X)
	-rm -f $(X)/libmips_$(X).a
	cd $(X)/. ; $(MAKE) $(MAKEARGS) -f ../Makefile \
		SYSTYPE="-systype $(X)" libmips.a
	-ln $(X)/libmips.a libmips_$(X).a

libmips.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

$(OBJS): $(SRCS) $(HDRS)

$(SRCS) $(HDRS):
	[ -r $@ ] || ln -s ../$@ .

clean:
	-rm -f $(OBJS)
	for i in $(VERSIONDIRS) ; \
	{ \
		rm -rf $$i ; \
	}
	
clobber: clean
	-rm -f $(VERSIONS)

all:	$(VERSIONS)

generic: $(VERSIONDIRS)
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)
	for i in $(VERSIONDIRS) ; \
	{ \
		cd $$i ; \
		$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
			$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) ; \
	}

install: all


