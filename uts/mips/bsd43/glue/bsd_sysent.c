/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: bsd_sysent.c,v 1.7.1.4.1.4 90/07/12 14:31:50 hawkes Exp $"

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)init_sysent.c	7.1 (Berkeley) 6/5/86
 */

/*
 * System call table for "-systype bsd43" system calls.
 *
 * The sysent array is in the exact same order as UMIPS-BSD, to simplify
 * possible UMIPS-BSD binary compatibility.  
 *
 * Currently I have left all of the names in the table just as they 
 * are in BSD.
 * For functions that we do not yet support, I have added
 *
 *	#define foo errsys
 *
 * For functions that aren't exactly like the SYSV counterpart, I've added
 *
 *	#define foo bsd_foo
 *
 * to avoid naming conflicts.  These bsd_* functions live in code under the
 * numips/bsd part of the kernel tree.  Non prefixed functions live in the
 * standard SYSV part of the tree.
 */

#include "sys/param.h"
#include "sys/types.h"
#include "sys/systm.h"

#ifndef DEBUG
#   define debug nosys
#endif DEBUG

#ifndef TRACE
#   define vtrace nosys
#endif

/*******************************************************************************
NOTES:
    > vfork()	Now implemented, but page table is not shared between child
		and parent, so if want leave junk from child to parent that
		way, you lose.
    > read()	Mapped to SYSV read.  RESTART and NDELAY return values broke.
    > write()	Mapped to SYSV read.  RESTART and NDELAY return values broke.
    > execve()	mapped to exece() -- might want to check ...
    > execv()	mapped to exec() -- might want to check ...
    > exit()    mapped to exit() -- make changes for pgrp handling like BSD

*******************************************************************************/

/*
 * The following functions are not yet supported, so they
 * get mapped to errsys().
 *
 * The goal is to get this list to zero.
 */
/* Priority 3 */
#   define debug errsys
#   define reboot errsys
#   define swapon errsys
#ifdef TRACE
#   define vtrace errsys
#endif TRACE

/*
 *	This following interface is not yet implemented in 4.3 BSD,
 *	and is simulated by libc.
 */

#define sbrk errsys

/*
 *	The following interfaces are obsolete, and only included in 
 *	4.3 BSD for binary compatibility with earlier systems.
 *	Therefore, they are not implemented in RISC/os, except as
 *	library routines.
 */

#   define oalarm errsys
#   define ofstat errsys
#   define oftime errsys
#   define omount errsys
#   define onice errsys
#   define opause errsys
#   define osetgid errsys
#   define osetpgrp errsys
#   define osetuid errsys
#   define ossig errsys
#   define ostat errsys
#   define ostime errsys
#   define otime errsys
#   define otimes errsys
#   define outime errsys
#   define ovadvise errsys
#   define ovlimit errsys
#   define ovtimes errsys
#   define owait errsys

/*
 * External definitions for all functions used.
 */
    extern int indir();
    extern int nosys();
    extern int nullsys();
    extern int errsys();

    extern int sethostid();
    extern int gethostid();
    extern int sethostname();
    extern int gethostname();
    extern int getpid();
    extern int setdomainname();
    extern int getdomainname();
    extern int fork();
    extern int vfork();
    extern int rexit();
    extern int exec();
    extern int exece();
    extern int wait3();
    extern int getuid();
    extern int setreuid();
    extern int getgid();
    extern int getgroups();
    extern int setregid();
    extern int setgroups();
    extern int bsd43_getpgrp();
    extern int bsd43_setpgrp();
    extern int sbreak();
    extern int sstk();
    extern int bsd_getpagesize();
    extern int mmap();
    extern int mremap();
    extern int munmap();
    extern int mprotect();
    extern int madvise();
    extern int mincore();
    extern int sigvec();
    extern int sigblock();
    extern int sigsetmask();
    extern int sigpause();
    extern int sigstack();
    extern int bsd_sigreturn();
    extern int bsd_kill();
    extern int killpg();
    extern int gettimeofday();
    extern int settimeofday();
    extern int getitimer();
    extern int setitimer();
    extern int adjtime();
    extern int bsd_getdtablesize();
    extern int dup();
    extern int dup2();
    extern int close();
    extern int select();
    extern int getdopt();
    extern int setdopt();
    extern int fcntl();
    extern int flock();
    extern int getpriority();
    extern int setpriority();
    extern int getrusage();
    extern int getrlimit();
    extern int setrlimit();
#if QUOTA
#   define oldquota nullsys
    extern int quotactl();
#else /* !QUOTA */
#   define oldquota nullsys
#   define quotactl errsys
#endif /* QUOTA */
    extern int unmount();
    extern int ufs_mount();
    extern int swapon();
    extern int smount();
    extern int omount();
    extern int umount();
    extern int sync();
    extern int reboot();
    extern int sysacct();
    extern int read();
    extern int write();
    extern int readv();
    extern int writev();
    extern int bsd43_ioctl();
    extern int chdir();
    extern int chroot();
    extern int mkdir();
    extern int rmdir();
    extern int getdirentries();
    extern int creat();
    extern int bsd43_open();
    extern int mknod();
    extern int unlink();
    extern int stat();
    extern int fstat();
    extern int lstat();
    extern int chown();
    extern int fchown();
    extern int chmod();
    extern int fchmod();
    extern int utimes();
    extern int link();
    extern int symlink();
    extern int readlink();
    extern int rename();
    extern int lseek();
    extern int truncate();
    extern int ftruncate();
    extern int access();
    extern int fsync();
    extern int statfs();
    extern int fstatfs();
    extern int socket();
    extern int bind();
    extern int listen();
    extern int accept();
    extern int connect();
    extern int socketpair();
    extern int sendto();
    extern int send();
    extern int recvfrom();
    extern int recv();
    extern int sendmsg();
    extern int recvmsg();
    extern int shutdown();
    extern int setsockopt();
    extern int getsockopt();
    extern int getsockname();
    extern int getpeername();
    extern int pipe();
    extern int umask();
    extern int ptrace();
    extern int owait();		/* now receive message on channel */
    extern int otime();		/* now use gettimeofday */
    extern int ostime();	/* now use settimeofday */
    extern int oalarm();	/* now use setitimer */
    extern int outime();	/* now use utimes */
    extern int opause();	/* now use sigpause */
    extern int onice();		/* now use setpriority,getpriority */
    extern int oftime();	/* now use gettimeofday */
    extern int osetpgrp();	/* ??? */
    extern int otimes();	/* now use getrusage */
    extern int ossig();		/* now use sigvec, etc */
    extern int ovlimit();	/* now use setrlimit,getrlimit */
    extern int ovtimes();	/* now use getrusage */
    extern int osetuid();	/* now use setreuid */
    extern int osetgid();	/* now use setregid */
    extern int ostat();		/* now use stat */
    extern int ofstat();	/* now use fstat */
    extern int profil();	/* 'cuz sys calls are interruptible */
    extern int vhangup();	/* should just do in exit() */
    extern int sbrk();		/* sbrk */
    extern int ovadvise();	/* awaiting new madvise */
    extern int bsd_sysmips();	/* MIPS specific syscall vector */
    extern int mipskopt();	/* get/set kernel options */
    extern int cacheflush();	/* flush cache */
    extern int cachectl();	/* change cacheability */
    extern int mipshwconf();	/* get/set hardware related config parameters */
#if NFSCLIENT
    extern int async_daemon();	/* client async daemon */
#else /* !NFSCLIENT */
#   define async_daemon	nosys
#endif /* NFSCLIENT */
#if NFSSERVER
    extern int nfs_svc();	/* run nfs server */
    extern int nfs_getfh();	/* get file handle */
    extern int old_exportfs();	/* export file systems */
    extern int exportfs();	/* NFS 4.0 export file systems */
#ifdef RISCOS
    extern int nfsfh_open();	/* NFS 4.0 external nfs handle open */
#endif
#else /* !NFSSERVER */
#   define nfs_svc	errsys
#   define nfs_getfh	nosys
#   define old_exportfs	errsys
#   define exportfs	errsys
#ifdef RISCOS
#define nfsfh_open      errsys
#endif
#endif /* NFSSERVER */
    extern int libattach(), libdetach();

#define	compat(n, name)	n, o/**/name

/*
 * Reserved/unimplemented system calls in the range 0-150 inclusive
 * are reserved for use in future Berkeley releases.
 * Additional system calls implemented in vendor and other
 * redistributions should be placed in the reserved range at the end
 * of the current calls.
 */
struct sysent bsd_sysent[] = {
    0, indir,		SY_SETJMP,		/*   0 = indirect access */
    1, rexit,		SY_SETJMP,		/*   1 = exit */
    0, fork,		SY_SETJMP,		/*   2 = fork */
    3, read,		SY_SETJMP,		/*   3 = read */
    3, write,		SY_SETJMP,		/*   4 = write */
    3, bsd43_open,	SY_SETJMP,		/*   5 = open */
    1, close,		SY_SETJMP,		/*   6 = close */
    compat(0,wait),	SY_SETJMP,		/*   7 = old wait */
    2, creat,		SY_SETJMP,		/*   8 = creat */
    2, link,		SY_SETJMP,		/*   9 = link */
    1, unlink,		SY_SETJMP,		/*  10 = unlink */
    2, exec,		SY_SETJMP,		/*  11 = execv */
    1, chdir,		SY_SETJMP,		/*  12 = chdir */
    compat(0,time),	SY_SETJMP,		/*  13 = old time */
    3, mknod,		SY_SETJMP,		/*  14 = mknod */
    2, chmod,		SY_SETJMP,		/*  15 = chmod */
    3, chown,		SY_SETJMP,		/*  16 = chown; now 3 args */
    1, sbreak,		SY_SETJMP,		/*  17 = old break (brk()) */
    compat(2,stat),	SY_SETJMP,		/*  18 = old stat */
    3, lseek,		SY_SETJMP,		/*  19 = lseek */
    0, getpid,		SY_SETJMP,		/*  20 = getpid */
    3, omount,		SY_SETJMP,		/*  21 = old mount */
    1, umount,		SY_SETJMP,		/*  22 = old umount */
    compat(1,setuid),	SY_SETJMP,		/*  23 = old setuid */
    0, getuid,		SY_SETJMP,		/*  24 = getuid */
    compat(1,stime),	SY_SETJMP,		/*  25 = old stime */
    4, ptrace,		SY_SETJMP,		/*  26 = ptrace */
    compat(1,alarm),	SY_SETJMP,		/*  27 = old alarm */
    compat(2,fstat),	SY_SETJMP,		/*  28 = old fstat */
    compat(0,pause),	SY_SETJMP,		/*  29 = opause */
    compat(2,utime),	SY_SETJMP,		/*  30 = old utime */
    0, nosys,		SY_SETJMP,		/*  31 = was stty */
    0, nosys,		SY_SETJMP,		/*  32 = was gtty */
    2, access,		SY_SETJMP,		/*  33 = access */
    compat(1,nice),	SY_SETJMP,		/*  34 = old nice */
    compat(1,ftime),	SY_SETJMP,		/*  35 = old ftime */
    0, sync,		SY_SETJMP,		/*  36 = sync */
    2, bsd_kill,	SY_SETJMP,		/*  37 = kill */
    2, stat,		SY_SETJMP,		/*  38 = stat */
    compat(2,setpgrp),	SY_SETJMP,		/*  39 = old setpgrp */
    2, lstat,		SY_SETJMP,		/*  40 = lstat */
    2, dup,		SY_SETJMP,		/*  41 = dup */
    0, pipe,		SY_SETJMP,		/*  42 = pipe */
    compat(1,times),	SY_SETJMP,		/*  43 = old times */
    4, profil,		SY_SETJMP,		/*  44 = profil */
    0, nosys,		SY_SETJMP,		/*  45 = nosys */
    compat(1,setgid),	SY_SETJMP,		/*  46 = old setgid */
    0, getgid,		SY_SETJMP,		/*  47 = getgid */
    compat(2,ssig),	SY_SETJMP,		/*  48 = old sig */
    0, nosys,		SY_SETJMP,		/*  49 = reserved for USG */
    0, nosys,		SY_SETJMP,		/*  50 = reserved for USG */
    1, sysacct,		SY_SETJMP,		/*  51 = turn acct off/on */
    0, nosys,		SY_SETJMP,		/*  52 = old set phys addr */
    0, nosys,		SY_SETJMP,		/*  53 = old lock in core */
    3, bsd43_ioctl,	SY_SETJMP,	        /*  54 = ioctl */
    1, reboot,		SY_SETJMP,		/*  55 = reboot */
    0, nosys,		SY_SETJMP,		/*  56 = old mpxchan */
    2, symlink,		SY_SETJMP,		/*  57 = symlink */
    3, readlink,	SY_SETJMP,		/*  58 = readlink */
    3, exece,		SY_SETJMP,		/*  59 = execve */
    1, umask,		SY_SETJMP,		/*  60 = umask */
    1, chroot,		SY_SETJMP,		/*  61 = chroot */
    2, fstat,		SY_SETJMP,		/*  62 = fstat */
    0, nosys,		SY_SETJMP,		/*  63 = reserved */
    0, bsd_getpagesize,	0,			/*  64 = getpagesize */
    5, mremap,		SY_SETJMP,		/*  65 = mremap */
    0, vfork,		SY_SETJMP,		/*  66 = vfork */
    0, read,		SY_SETJMP,		/*  67 = old vread */
    0, write,		SY_SETJMP,		/*  68 = old vwrite */
    1, sbrk,		SY_SETJMP,		/*  69 = sbrk */
    1, sstk,		SY_SETJMP,		/*  70 = sstk */
    6, mmap,		SY_SETJMP,		/*  71 = mmap */
    1, ovadvise,	SY_SETJMP,		/*  72 = old vadvise */
    2, munmap,		SY_SETJMP,		/*  73 = munmap */
    3, mprotect,	SY_SETJMP,		/*  74 = mprotect */
    3, madvise,		SY_SETJMP,		/*  75 = madvise */
    1, vhangup,		SY_SETJMP,		/*  76 = vhangup */
    compat(2,vlimit),	SY_SETJMP,		/*  77 = old vlimit */
    3, mincore,		SY_SETJMP,		/*  78 = mincore */
    2, getgroups,	SY_SETJMP,		/*  79 = getgroups */
    2, setgroups,	SY_SETJMP,		/*  80 = setgroups */
    1, bsd43_getpgrp,	SY_SETJMP,		/*  81 = getpgrp */
    2, bsd43_setpgrp,	SY_SETJMP,		/*  82 = setpgrp */
    3, setitimer,	SY_SETJMP,		/*  83 = setitimer */
/*
 * This is the general wait, wait3 entry point. wait will also have
 * 3 arguments with the last 2 being zero.
 */
    3, wait3,		SY_SETJMP,		/*  84 = wait3 */
    1, swapon,		SY_SETJMP,		/*  85 = swapon */
    2, getitimer,	SY_SETJMP,		/*  86 = getitimer */
    2, gethostname,	SY_SETJMP,		/*  87 = gethostname */
    2, sethostname,	SY_SETJMP,		/*  88 = sethostname */
    0, bsd_getdtablesize, 0,			/*  89 = getdtablesize */
    2, dup2,		SY_SETJMP,		/*  90 = dup2 */
    2, getdopt,		SY_SETJMP,		/*  91 = getdopt */
    3, fcntl,		SY_SETJMP,		/*  92 = fcntl */
    5, select,		SY_SETJMP,		/*  93 = select */
    2, setdopt,		SY_SETJMP,		/*  94 = setdopt */
    1, fsync,		SY_SETJMP,		/*  95 = fsync */
    3, setpriority,	SY_SETJMP,		/*  96 = setpriority */
    3, socket,		SY_SETJMP,		/*  97 = socket */
    3, connect,		SY_SETJMP,		/*  98 = connect */
    3, accept,		SY_SETJMP,		/*  99 = accept */
    2, getpriority,	SY_SETJMP,		/* 100 = getpriority */
    4, send,		SY_SETJMP,		/* 101 = send */
    4, recv,		SY_SETJMP,		/* 102 = recv */
    1, bsd_sigreturn,	SY_SETJMP,		/* 103 = sigreturn */
    3, bind,		SY_SETJMP,		/* 104 = bind */
    5, setsockopt,	SY_SETJMP,		/* 105 = setsockopt */
    2, listen,		SY_SETJMP,		/* 106 = listen */
    compat(2,vtimes),	SY_SETJMP,		/* 107 = old vtimes */
    4, sigvec,		SY_SETJMP,		/* 108 = sigvec */
    1, sigblock,	SY_SETJMP,		/* 109 = sigblock */
    1, sigsetmask,	SY_SETJMP,		/* 110 = sigsetmask */
    1, sigpause,	SY_SETJMP,		/* 111 = sigpause */
    2, sigstack,	SY_SETJMP,		/* 112 = sigstack */
    3, recvmsg,		SY_SETJMP,		/* 113 = recvmsg */
    3, sendmsg,		SY_SETJMP,		/* 114 = sendmsg */
    2, vtrace,		SY_SETJMP,		/* 115 = vtrace */
    2, gettimeofday,	SY_SETJMP,		/* 116 = gettimeofday */
    2, getrusage,	SY_SETJMP,		/* 117 = getrusage */
    5, getsockopt,	SY_SETJMP,		/* 118 = getsockopt */
    0, nosys,		SY_SETJMP,		/* 119 = nosys */
    3, readv,		SY_SETJMP,		/* 120 = readv */
    3, writev,		SY_SETJMP,		/* 121 = writev */
    2, settimeofday,	SY_SETJMP,		/* 122 = settimeofday */
    3, fchown,		SY_SETJMP,		/* 123 = fchown */
    2, fchmod,		SY_SETJMP,		/* 124 = fchmod */
    6, recvfrom,	SY_SETJMP,		/* 125 = recvfrom */
    2, setreuid,	SY_SETJMP,		/* 126 = setreuid */
    2, setregid,	SY_SETJMP,		/* 127 = setregid */
    2, rename,		SY_SETJMP,		/* 128 = rename */
    2, truncate,	SY_SETJMP,		/* 129 = truncate */
    2, ftruncate,	SY_SETJMP,		/* 130 = ftruncate */
    2, flock,		SY_SETJMP,		/* 131 = flock */
    0, nosys,		SY_SETJMP,		/* 132 = nosys */
    6, sendto,		SY_SETJMP,		/* 133 = sendto */
    2, shutdown,	SY_SETJMP,		/* 134 = shutdown */
    5, socketpair,	SY_SETJMP,		/* 135 = socketpair */
    2, mkdir,		SY_SETJMP,		/* 136 = mkdir */
    1, rmdir,		SY_SETJMP,		/* 137 = rmdir */
    2, utimes,		SY_SETJMP,		/* 138 = utimes */
    1, bsd_sigreturn,	SY_SETJMP, /*~~~SIGRET?	/* 139 = sigreturn (4.2 longjmps) */
    2, adjtime,		SY_SETJMP,		/* 140 = adjtime */
    3, getpeername,	SY_SETJMP,		/* 141 = getpeername */
    2, gethostid,	SY_SETJMP,		/* 142 = gethostid */
    1, sethostid,	SY_SETJMP,		/* 143 = sethostid - XXX with NFS */
    2, getrlimit,	SY_SETJMP,		/* 144 = getrlimit */
    2, setrlimit,	SY_SETJMP,		/* 145 = setrlimit */
    2, killpg,		SY_SETJMP,		/* 146 = killpg */
    0, nosys,		SY_SETJMP,		/* 147 = nosys */
    0, oldquota,	SY_SETJMP,/* XXX */	/* 148 = old quota */
    0, oldquota,	SY_SETJMP,/* XXX */	/* 149 = old qquota */
    3, getsockname,	SY_SETJMP,		/* 150 = getsockname */
    /*
     * Syscalls 151-180 inclusive are reserved for vendor-specific
     * system calls.  (This includes various calls added for compatibity
     * with other Unix variants.)
     */
    5, bsd_sysmips,	SY_SETJMP,		/* 151 = sysmips */
    3, cacheflush,	SY_SETJMP,		/* 152 = cacheflush */
    3, cachectl,	SY_SETJMP,		/* 153 = cachectl */
    1, debug,		SY_SETJMP,		/* 154 = debug */
    0, nosys,		SY_SETJMP,		/* 155 = nosys */
    0, nosys,		SY_SETJMP,		/* 156 = nosys */
    0, nosys,		SY_SETJMP,		/* 157 = old nfs_mount */
    1, nfs_svc,		SY_SETJMP,		/* 158 = nfs_svc */
    4, getdirentries,	SY_SETJMP,		/* 159 = getdirentries */
    2, statfs,		SY_SETJMP,		/* 160 = statfs */
    2, fstatfs,		SY_SETJMP,		/* 161 = fstatfs */
    1, unmount,		SY_SETJMP,		/* 162 = unmount */
    0, async_daemon,	SY_SETJMP,		/* 163 = async_daemon */
    2, nfs_getfh,	SY_SETJMP,		/* 164 = get file handle */
    2, getdomainname,	SY_SETJMP,		/* 165 = getdomainname */
    2, setdomainname,	SY_SETJMP,		/* 166 = setdomainname */
    0, nosys,		SY_SETJMP,		/* 167 = old pcfs_mount */
    4, quotactl,	SY_SETJMP,		/* 168 = quotactl */
    3, old_exportfs,	SY_SETJMP,		/* 169 = exportfs */
    4, smount,		SY_SETJMP,		/* 170 = mount */
    2, mipshwconf,	SY_SETJMP,		/* 171 = mipshwconf */
    2, exportfs,	SY_SETJMP,		/* 172 = NFS 4.0 exportfs */
    3, nfsfh_open,	SY_SETJMP,		/* 173 = NFS 4.0 ext fh open */
    3, libattach,	SY_SETJMP,		/* 174 = libattach */
    1, libdetach,	SY_SETJMP,		/* 175 = libdetach */

    /* Additional Reservations: 
     *		176..250	MIPS
     *		251..275	CDC
     *
     * Note well:  You must add dummy table entries ("0, nosys, 0,")
     * for all entries preceding those which you change to other values,
     * since this table is directly indexed by the system call number.
     */

    /*
     *	BEFORE ADDING SYSTEM CALLS, CUSTOMERS SHOULD CONTACT MIPS
     *	TO GET A RESERVED BLOCK OF SYSTEM CALL NUMBERS.
     *
     *	This will help MIPS maintain binary compatibility for
     *	programs that don't use customer specific system calls.
     *
     *  Currently, reserved sysno's are marked MIPS and CDC.
     */
};

int	bsd_nsysent = sizeof (bsd_sysent) / sizeof (bsd_sysent[0]);
