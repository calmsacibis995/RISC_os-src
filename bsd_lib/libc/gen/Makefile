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
# $Header: Makefile,v 1.11.1.3 90/05/07 20:33:36 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=abort.o alarm.o alloca.o atof.o atoi.o atol.o bcopy.o bzero.o \
	calloc.o closedir.o crypt.o ctime.o ctype_.o ecvt.o errlst.o \
	execvp.o exportent.o fakcu.o frexp.o fstab.o gcvt.o getenv.o getgrent.o \
	getlogin.o getpass.o getpwent.o getttyent.o getttynam.o \
	getusershell.o getwd.o index.o initgroups.o isatty.o ldexp.o \
	malloc.o mktemp.o mntent.o modf.o ndbm.o opendir.o ovbcopy.o \
	perror.o popen.o psignal.o qsort.o random.o readdir.o realpath.o regex.o \
	rindex.o scandir.o seekdir.o setegid.o setenv.o seteuid.o \
	setgid.o setrgid.o setruid.o setuid.o siginterrupt.o siglist.o \
	signal.o sleep.o strcat.o strcmp.o strcpy.o strncat.o \
	strncmp.o strncpy.o swab.o syslog.o system.o telldir.o time.o \
	timezone.o ttyname.o ttyslot.o ualarm.o valloc.o getgrgid.o \
	getgrnam.o strcasecmp.o usleep.o

SRCS	=abort.c alarm.c alloca.c atof.c atoi.c atol.c bcopy.c bzero.c \
	calloc.c closedir.c crypt.c ctime.c ctype_.c ecvt.c errlst.c \
	execvp.c exportent.c fakcu.c frexp.c fstab.c gcvt.c getenv.c getgrent.c \
	getlogin.c getpass.c getpwent.c getttyent.c getttynam.c \
	getusershell.c getwd.c index.c initgroups.c isatty.c ldexp.c \
	mktemp.c mntent.c modf.c ndbm.c opendir.c ovbcopy.c \
	popen.c psignal.c qsort.c random.c readdir.c realpath.c regex.c \
	rindex.c scandir.c seekdir.c setegid.c setenv.c seteuid.c \
	setgid.c setrgid.c setruid.c setuid.c siginterrupt.c siglist.c \
	signal.c sleep.c strcat.c strcmp.c strcpy.c strncat.c \
	strncmp.c strncpy.c swab.c syslog.c system.c telldir.c time.c \
	timezone.c ttyname.c ttyslot.c ualarm.c valloc.c getgrgid.c \
	getgrnam.c strcasecmp.c usleep.c

HDRS	=

SYSVGENSRCS =errlst.c malloc.c perror.c
SYSVSRCS =$(SYSVGENSRCS)

WHOAMI	=libc_gen.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

$(INTEGDIR)/$(WHOAMI): $(WHOAMI)
	-rm -f $(INTEGDIR)/$(WHOAMI)
	cp $(WHOAMI) $(INTEGDIR)

$(WHOAMI): $(OBJS)
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)
	-rm -f $(SYSVSRCS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

$(SYSVGENSRCS): ../../../lib/libc/gen/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../lib/libc/gen/$@ $@
