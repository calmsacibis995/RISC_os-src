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
# $Header: Makefile,v 1.6.2.2 90/05/10 02:33:57 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/usr/lib

OBJS	=Err.o basename.o bgets.o bufsplit.o copylist.o day.o dirname.o \
	eaccess.o erraction.o errafter.o errbefore.o errexit.o errprefix.o \
	errsource.o errstrtok.o errtag.o errtext.o errtofix.o errusage.o \
	gmatch.o hash.o incount.o isencrypt.o julian.o leap.o mkdirp.o \
	num.o numd.o p2open.o pathfind.o regerror.o rmdirp.o sesystem.o \
	sgets.o smemcpy.o sputs.o stradd.o strccpy.o strecnt.o strecpy.o \
	strnlen.o sweekday.o tconv.o tconvs.o tmpgetopt.o to_date.o tree.o \
	triml.o trimt.o waitpid.o weekday.o zaccess.o zacct.o zcalloc.o \
	zchdir.o zchmod.o zchown.o zchroot.o zclose.o zcreat.o zcuserid.o \
	zdup.o zexecv.o zexecve.o zexecvp.o zfclose.o zfcntl.o zfdopen.o \
	zferror.o zfopen.o zfread.o zfreopen.o zfseek.o zfstat.o zfwrite.o \
	zioctl.o zkill.o zlink.o zlseek.o zmalloc.o zmknod.o zmount.o \
	znice.o zopen.o zpipe.o zplock.o zptrace.o zread.o zrealloc.o \
	zsetgid.o zsetuid.o zstat.o zulimit.o zumount.o zuname.o zunlink.o \
	zwait.o zwrite.o

SRCS	=Err.c basename.c bgets.c bufsplit.c copylist.c day.c dirname.c \
	eaccess.c erraction.c errafter.c errbefore.c errexit.c errprefix.c \
	errsource.c errstrtok.c errtag.c errtext.c errtofix.c errusage.c \
	gmatch.c hash.c incount.c isencrypt.c julian.c leap.c mkdirp.c \
	num.c numd.c p2open.c pathfind.c regerror.c rmdirp.c sesystem.c \
	sgets.c smemcpy.c sputs.c stradd.c strccpy.c strecnt.c strecpy.c \
	strnlen.c sweekday.c tconv.c tconvs.c tmpgetopt.c to_date.c tree.c \
	triml.c trimt.c waitpid.c weekday.c zaccess.c zacct.c zcalloc.c \
	zchdir.c zchmod.c zchown.c zchroot.c zclose.c zcreat.c zcuserid.c \
	zdup.c zexecv.c zexecve.c zexecvp.c zfclose.c zfcntl.c zfdopen.c \
	zferror.c zfopen.c zfread.c zfreopen.c zfseek.c zfstat.c zfwrite.c \
	zioctl.c zkill.c zlink.c zlseek.c zmalloc.c zmknod.c zmount.c \
	znice.c zopen.c zpipe.c zplock.c zptrace.c zread.c zrealloc.c \
	zsetgid.c zsetuid.c zstat.c zulimit.c zumount.c zuname.c zunlink.c \
	zwait.c zwrite.c

HDRS	=errmsg.h hash.h libgen.h strselect.h

LIBSUFFIX=
WHOAMI	=libgen$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

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
