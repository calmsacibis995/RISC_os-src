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
# $Header: Makefile,v 1.3.3.2.1.2 90/07/23 10:18:19 hawkes Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=Ovadvise.o Ovfork.o _exit.o accept.o access.o acct.o adjtime.o \
	async_daemon.o bind.o brk.o cachectl.o cacheflush.o cerror.o \
	chdir.o chmod.o chown.o chroot.o close.o connect.o creat.o dup.o \
	dup2.o execl.o execle.o exect.o execv.o execve.o exportfs.o \
	fchmod.o fchown.o fcntl.o fixade.o flock.o fork.o fp_sigintr.o \
	fstat.o fstatfs.o fsync.o ftruncate.o getdirentries.o \
	getdomainname.o getdtablesize.o getegid.o geteuid.o getfh.o \
	getgid.o getgroups.o gethostid.o gethostname.o getitimer.o \
	getpagesize.o getpeername.o getpgrp.o getpid.o getppid.o \
	getpriority.o getrlimit.o getrusage.o getsockname.o getsockopt.o \
	gettimeofday.o getuid.o hwconf.o ioctl.o kill.o killpg.o kopt.o \
	link.o listen.o lseek.o lstat.o m_getrusage.o m_wait3.o mipsfpu.o \
	mkdir.o mknod.o mmap.o mount.o munmap.o nfssvc.o omount.o open.o \
	oumount.o pipe.o profil.o ptrace.o quota.o quotactl.o read.o \
	readlink.o readv.o reboot.o recv.o recvfrom.o recvmsg.o rename.o \
	rmdir.o sbrk.o select.o send.o sendmsg.o sendto.o setdomainname.o \
	setgroups.o sethostid.o sethostname.o setitimer.o setpgrp.o \
	setpriority.o setquota.o setregid.o setreuid.o setrlimit.o \
	setsockopt.o settimeofday.o shutdown.o sigblock.o sigpause.o \
	sigreturn.o sigsetmask.o sigstack.o socket.o socketpair.o \
	stat.o statfs.o swapon.o symlink.o sync.o syscall.o truncate.o \
	umask.o uname.o unlink.o unmount.o utimes.o vhangup.o wait.o \
	wait3.o write.o writev.o sigvec.o libattach.o libdetach.o

SRCS	=Ovadvise.s Ovfork.s _exit.s accept.s access.s acct.s adjtime.s \
	async_daemon.s bind.s brk.s cachectl.s cacheflush.s cerror.s \
	chdir.s chmod.s chown.s chroot.s close.s connect.s creat.s dup.s \
	dup2.s execl.s execle.s exect.s execv.s execve.s exportfs.s \
	fchmod.s fchown.s fcntl.s fixade.s flock.s fork.s fp_sigintr.s \
	fstat.s fstatfs.s fsync.s ftruncate.s getdirentries.s \
	getdomainname.s getdtablesize.s getegid.s geteuid.s getfh.s \
	getgid.s getgroups.s gethostid.s gethostname.s getitimer.s \
	getpagesize.s getpeername.s getpgrp.s getpid.s getppid.s \
	getpriority.s getrlimit.s getrusage.c getsockname.s getsockopt.s \
	gettimeofday.s getuid.s hwconf.s ioctl.s kill.s killpg.s kopt.s \
	link.s listen.s lseek.s lstat.s m_getrusage.s m_wait3.s mipsfpu.s \
	mkdir.s mknod.s mmap.s mount.s munmap.s nfssvc.s omount.s open.s \
	oumount.s pipe.s profil.s ptrace.s quota.s quotactl.s read.s \
	readlink.s readv.s reboot.s recv.s recvfrom.s recvmsg.s rename.s \
	rmdir.s sbrk.s select.s send.s sendmsg.s sendto.s setdomainname.s \
	setgroups.s sethostid.s sethostname.s setitimer.s setpgrp.s \
	setpriority.s setquota.s setregid.s setreuid.s setrlimit.s \
	setsockopt.s settimeofday.s shutdown.s sigblock.s sigpause.s \
	sigreturn.s sigsetmask.s sigstack.s socket.s socketpair.s \
	stat.s statfs.s swapon.s symlink.s sync.s syscall.s truncate.s \
	umask.s uname.s unlink.s unmount.s utimes.s vhangup.s wait.s \
	wait3.c write.s writev.s sigvec.s libattach.s libdetach.s

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

.s.o:
	$(CC) $(CFLAGS) -c $<
