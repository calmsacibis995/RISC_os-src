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
# $Header: Makefile,v 1.5.1.2 90/05/10 04:17:11 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=-systype posix $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype posix $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=access.o alarm.o chdir.o chmod.o chown.o close.o creat.o \
	dup.o dup2.o execl.o execle.o execv.o execve.o exit.o \
	fcntl.o fork.o fstat.o ftruncate.o  \
	getdents.o getegid.o geteuid.o getgid.o getgroups.o \
	__getmaxsig.o getpagesize.o \
	getpgrp.o getpid.o getppid.o getuid.o \
	ioctl.o kill.o link.o lseek.o lstat.o \
	mkdir.o mknod.o open.o pause.o pipe.o \
	read.o rename.o rmdir.o sbrk.o \
	setgid.o setpgid.o setsid.o setuid.o \
	sigaction.o sigpending.o sigprocmask.o sigreturn.o \
	sigsetjmp.o sigsuspend.o stat.o sysmips.o \
	time.o times.o truncate.o umask.o uname.o unlink.o utime.o \
	wait.o write.o waitpid.o 

SRCS	=dup2.s getgroups.s getpgrp.s \
	__getmaxsig.s setsid.s sigaction.s sigpending.s sigprocmask.s \
	sigsetjmp.s sigsuspend.s waitpid.s

SYSVSYSMIPSSRCS=access.s alarm.s chdir.s chmod.s chown.s close.s creat.s \
	dup.s execl.s execle.s execv.s execve.s exit.s \
	fcntl.s fork.s fstat.s ftruncate.s \
	getdents.s getegid.s geteuid.s getgid.s getpagesize.s \
	getpid.s getppid.s getuid.s \
	ioctl.s kill.s link.s lseek.s lstat.s mkdir.s mknod.s \
	open.s pause.s pipe.s read.s rename.s rmdir.s sbrk.s \
	setgid.s setpgid.s setuid.s sigreturn.s stat.s sysmips.s \
	time.s times.s truncate.s \
	umask.s uname.s unlink.s utime.s wait.s write.s 

SYSVSRCS=$(SYSVSYSMIPSSRCS)

HDRS	=

WHOAMI	=libc_sys.mips.a
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

$(SYSVSYSMIPSSRCS): ../../../lib/libc/sys.mips/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../lib/libc/sys.mips/$@ $@
