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
# $Header: Makefile,v 1.13.1.3.1.2 90/07/23 10:19:24 hawkes Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(GNUM) -systype sysv $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(GNUM) -systype sysv $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=access.o acct.o advfs.o alarm.o chdir.o chmod.o chown.o chroot.o \
	close.o creat.o dup.o execl.o execle.o execv.o execve.o exit.o \
	fcntl.o fork.o fstat.o fstatfs.o ftruncate.o getdents.o \
	getegid.o geteuid.o getgid.o getmsg.o getpid.o getppid.o getuid.o \
	ioctl.o kill.o link.o lseek.o lstat.o mkdir.o mknod.o \
	mount.o netboot.o netunboot.o nice.o open.o pause.o pipe.o plock.o \
	poll.o profil.o ptrace.o putmsg.o rdebug.o rdump.o read.o \
	readlink.o rename.o rfstart.o rfstop.o rfsys.o rmdir.o rmount.o \
	rumount.o sbrk.o setgid.o setpgrp.o setuid.o signal.o sigreturn.o \
	stat.o statfs.o stime.o symlink.o sync.o syscall.o sysfs.o \
	sysmips.o time.o times.o truncate.o uadmin.o ulimit.o umask.o \
	umount.o unadvfs.o uname.o unlink.o ustat.o utime.o wait.o write.o \
	cacheflush.o cachectl.o fchown.o fchmod.o getitimer.o setitimer.o \
	wait2.o mmap.o munmap.o madvise.o getpagesize.o setpgid.o \
	libattach.o libdetach.o

SRCS	=access.s acct.s advfs.s alarm.s chdir.s chmod.s chown.s chroot.s \
	close.s creat.s dup.s execl.s execle.s execv.s execve.s exit.s \
	fcntl.s fork.s fstat.s fstatfs.s ftruncate.s getdents.s \
	getegid.s geteuid.s getgid.s getmsg.s getpid.s getppid.s getuid.s \
	ioctl.s kill.s link.s lseek.s lstat.s mkdir.s mknod.s \
	mount.s netboot.s netunboot.s nice.s open.s pause.s pipe.s plock.s \
	poll.s profil.s ptrace.s putmsg.s rdebug.s rdump.s read.s \
	readlink.s rename.s rfstart.s rfstop.s rfsys.s rmdir.s rmount.s \
	rumount.s sbrk.s setgid.s setpgrp.s setuid.s signal.s sigreturn.s \
	stat.s statfs.s stime.s symlink.s sync.s syscall.s sysfs.s \
	sysmips.s time.s times.s truncate.s uadmin.s ulimit.s umask.s \
	umount.s unadvfs.s uname.s unlink.s ustat.s utime.s wait.s write.s \
	cacheflush.s cachectl.s fchown.s fchmod.s getitimer.s setitimer.s \
	wait2.s mmap.s munmap.s madvise.s getpagesize.s setpgid.s \
	libattach.s libdetach.s

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

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)
