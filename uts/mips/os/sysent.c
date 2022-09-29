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
#ident	"$Header: sysent.c,v 1.32.1.5.1.3 90/07/12 10:42:12 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/param.h"
#include "sys/types.h"
#include "sys/systm.h"

/*
 * This table is the switch used to transfer
 * to the appropriate routine for processing a system call.
 * Each row contains the number of arguments expected,
 * a switch that tells systrap() in trap.c whether a setjmp() is not necessary,
 * and a pointer to the routine.
 */

int	alarm();
int	chdir();
int	chmod();
int	chown();
int	chroot();
int	close();
int	creat();
int	dup();
int	exec();
int	exece();
int	fcntl();
int	ofcntl();
int	fork();
int	fstat();
int	getgid();
int	getpid();
int	getuid();
int	gtime();
int	gtty();
int	ino_open();
int	ioctl();
int	indir();
int	kill();
int	link();
int	lock();
int	locking();	/* file locking hook -- Sept 1980, John Bass */
int	locsys();
int	mknod();
int	msgsys();
int	nice();
int	nosys();
int	nullsys();
int	open();
int	pause();
int	sysv_pipe();
int	profil();
int	ptrace();
int	read();
int	rexit();
int	access();
int	sbreak();
int	lseek();
int	semsys();
int	setgid();
int	setpgrp();
int	setuid();
int	shmsys();
int	sysv_smount();
int	ssig();
int	stat();
int	stime();
int	stty();
int	sumount();
int	sync();
int	sysacct();
int	times();
#ifdef MEGA
int	uexch();
#endif /* MEGA */
int	ulimit();
int	umask();
int	unlink();
int	utime();
int	utssys();
int	wait();
int	wait3();	/* Common interface for wait(), wait2(), wait3() */
int	write();
int 	advfs();
int	rfstart();
int 	rfstop();
int 	rdebug();
int 	rmount();
int 	rumount();
int	rfsys();
int	unadvfs();
int	rmdir();
int	mkdir();
int	getdirentries();
int	sysv_statfs();
int	sysv_fstatfs();
int	sysfs();
int	getmsg();
int	poll();
int	putmsg();
int	sysmips();
int	uadmin();
int	sigreturn();

/* 4.2 compatible socket/TCP/IP system calls */
int	accept();
int	bind();
int	connect();
int	getdtablesize();
int	gethostid();
int	getpagesize();
int	getpeername();
int	getsockname();
int	getsockopt();
int	listen();
int	recv();
int	recvfrom();
int	recvmsg();
int	select();
int	send();
int	sendmsg();
int	sendto();
int	sethostid();
int	setsockopt();
int	shutdown();
int	socket();
int	gethostname();
int	sethostname();
int	getdomainname();
int	setdomainname();

int	truncate();
int	ftruncate();
int	rename();
int	symlink();
int	readlink();
int	lstat();

int	cacheflush();
int	cachectl();
int	adjtime();
int	getitimer();
int	setitimer();
int	fchown();
int	fchmod();

int	mmap();
int	munmap();
int	madvise();

int	setreuid();
int	setregid();

int	setpgid();
int	getgroups();
int	setgroups();
int	gettimeofday();
int	getrlimit();
int	setrlimit();
int	getrusage();

int	libattach(), libdetach();

#ifdef NFSCLIENT
/* nfs */
int	async_daemon();		/* client async daemon */
#if RISCOS
int	nfsmount();		/* backward-compatible nfs-specific mount */
#else
#define nfsmount	nosys
#endif /* RISCOS */
#else /* !NFSCLIENT */
int	errsys();
#define async_daemon	errsys	/* change to errsys, so biod won't core dump */
#define nfsmount	nosys
#endif /* NFSCLIENT */
#ifdef NFSSERVER
int	nfs_svc();		/* run nfs server */
int	nfs_getfh();		/* get file handle */
int	old_exportfs();		/* export file systems */
int	exportfs();		/* NFS 4.0 export file systems */
#else
#define nfs_svc		nosys
#define nfs_getfh	nosys
#define old_exportfs	nosys
#define exportfs	nosys
#endif /* NFSSERVER */

struct sysent sysent[] =
{
    0, indir,		0,			/*  0 = indir */
    1, rexit,		SY_SETJMP,		/*  1 = exit */
    0, fork,		SY_SETJMP,		/*  2 = fork */
    3, read,		SY_SETJMP,		/*  3 = read */
    3, write,		SY_SETJMP,		/*  4 = write */
    3, open,		SY_SETJMP,		/*  5 = open */
    1, close,		SY_SETJMP,		/*  6 = close */
    0, wait,		SY_SETJMP,		/*  7 = wait */
    2, creat,		SY_SETJMP,		/*  8 = creat */
    2, link,		SY_SETJMP,		/*  9 = link */
    1, unlink,		SY_SETJMP,		/* 10 = unlink */
    2, exec,		SY_SETJMP,		/* 11 = exec */
    1, chdir,		SY_SETJMP,		/* 12 = chdir */
    0, gtime,		0,			/* 13 = time */
    3, mknod,		SY_SETJMP,		/* 14 = mknod */
    2, chmod,		SY_SETJMP,		/* 15 = chmod */
    3, chown,		SY_SETJMP,		/* 16 = chown; now 3 args */
    1, sbreak,		SY_SETJMP,		/* 17 = break */
    2, stat,		SY_SETJMP,		/* 18 = stat */
    3, lseek,		SY_SETJMP,		/* 19 = lseek */
    0, getpid,		0,			/* 20 = getpid */
    4, sysv_smount,	SY_SETJMP,		/* 21 = mount */
    1, sumount,		SY_SETJMP,		/* 22 = umount */
    1, setuid,		0,			/* 23 = setuid */
    0, getuid,		0,			/* 24 = getuid */
    1, stime,		0,			/* 25 = stime */
    4, ptrace,		0,			/* 26 = ptrace */
    1, alarm,		0,			/* 27 = alarm */
    2, fstat,		SY_SETJMP,		/* 28 = fstat */
    0, pause,		SY_SETJMP,		/* 29 = pause */
    2, utime,		0,			/* 30 = utime */
#ifdef NOTDEF
    2, stty,		SY_SETJMP,		/* 31 = stty */
    2, gtty,		SY_SETJMP,		/* 32 = gtty */
#else
    2, nosys,		SY_SETJMP,		/* 31 = stty */
    2, nosys,		SY_SETJMP,		/* 32 = gtty */
#endif
    2, access,		SY_SETJMP,		/* 33 = access */
    1, nice,		0,			/* 34 = nice */
    4, sysv_statfs,	SY_SETJMP,		/* 35 = statfs */
    0, sync,		SY_SETJMP,		/* 36 = sync */
    2, kill,		SY_SETJMP,		/* 37 = kill */
    4, sysv_fstatfs,	SY_SETJMP,		/* 38 = fstatfs */
    1, setpgrp,		0,			/* 39 = setpgrp */
    0, nosys,		0,			/* 40 = SGI - syssgi() */
    1, dup,		0,			/* 41 = dup */
    0, sysv_pipe,	SY_SETJMP,		/* 42 = pipe */
    1, times,		0,			/* 43 = times */
    4, profil,		0,			/* 44 = prof */
    1, lock,		0,			/* 45 = proc lock */
    1, setgid,		0,			/* 46 = setgid */
    0, getgid,		0,			/* 47 = getgid */
    3, ssig,		SY_SETJMP,		/* 48 = sig */
    6, msgsys,		SY_SETJMP,		/* 49 = IPC message */
    4, sysmips,		0,			/* 50 = mips-specific syscall */
    1, sysacct,		0,			/* 51 = turn acct off/on */
    4, shmsys,		SY_SETJMP,		/* 52 = shared memory */
    5, semsys,		SY_SETJMP,		/* 53 = IPC semaphores */
    3, ioctl,		SY_SETJMP,		/* 54 = ioctl */
    3, uadmin,		SY_SETJMP,		/* 55 = uadmin */
#ifdef MEGA
    3, uexch,		0,			/* 56 = uexch */
#else
    0, nosys,		0,			/* 56 = reserved for exch */
#endif /* MEGA */
    3, utssys,		0,			/* 57 = utssys */
    0, nosys,		0,			/* 58 = reserved for USG */
    3, exece,		SY_SETJMP,		/* 59 = exece */
    1, umask,		0,			/* 60 = umask */
    1, chroot,		SY_SETJMP,		/* 61 = chroot */
    3, ofcntl,		SY_SETJMP,		/* 62 = pre-RISC/os 5.0 fcntl */
    2, ulimit,		0,			/* 63 = ulimit */

    /* The following 6 entries were reserved for Safari 4 */
    0, nosys,		0,			/* 64 +0 = nosys */
    0, nosys,		0,			/* 64 +1 = nosys */
    0, nosys,		0,			/* 64 +2 = nosys */
    0, nosys,		0,			/* 64 +3 = file locking call */
    0, nosys,		0,			/* 64 +4 = local system calls */
    0, nosys,		0,			/* 64 +5 = inode open */

    4, advfs,		0,			/* 70 = advfs */
    1, unadvfs,		0,			/* 71 = unadvfs */
    4, rmount,		0,			/* 72 = rmount */
    1, rumount,		0,			/* 73 = rumount */
    5, rfstart,		SY_SETJMP,		/* 74 = rfstart */
    0, nosys,		0,			/* 75 = not used */
    1, rdebug,		SY_SETJMP,		/* 76 = rdebug */
    0, rfstop,		0,			/* 77 = rfstop */
    6, rfsys,		0,			/* 78 = rfsys */

    1, rmdir,		SY_SETJMP,		/* 79 = rmdir */
    2, mkdir,		SY_SETJMP,		/* 80 = mkdir */
    4, getdirentries,	SY_SETJMP,		/* 81 = getdents */
    0, nosys,		0,			/* 82 = SGI - sginap */
    0, nosys,		0,			/* 83 = SGI - sgikopt */
    3, sysfs,		0,			/* 84 = sysfs */
    4, getmsg,		SY_SETJMP,		/* 85 = getmsg */
    4, putmsg,		SY_SETJMP,		/* 86 = putmsg */
    3, poll,		SY_SETJMP,		/* 87 = poll */

    /* This is an added system call for the mips signal return mechanism */
    1, sigreturn,	SY_SIGRET,		/* 88 = sig return and cleanup*/

    /* 4.2 compatible socket/TCP/IP system calls */
    3, accept,		SY_SETJMP,		/* 89 = 4.2 accept */
    3, bind,		SY_SETJMP,		/* 90 = 4.2 bind */
    3, connect,		SY_SETJMP,		/* 91 = 4.2 connect */
    0, gethostid,	SY_SETJMP,		/* 92 = 4.2 gethostid */
    3, getpeername,	SY_SETJMP,		/* 93 = 4.2 getpeername */
    3, getsockname,	SY_SETJMP,		/* 94 = 4.2 getsockname */
    5, getsockopt,	SY_SETJMP,		/* 95 = 4.2 getsockopt */
    2, listen,		SY_SETJMP,		/* 96 = 4.2 listen */
    4, recv,		SY_SETJMP,		/* 97 = 4.2 recv */
    6, recvfrom,	SY_SETJMP,		/* 98 = 4.2 recvfrom */
    3, recvmsg,		SY_SETJMP,		/* 99 = 4.2 recvmsg */
    5, select,		SY_SETJMP,		/* 100 = 4.2 select */
    4, send,		SY_SETJMP,		/* 101 = 4.2 send */
    3, sendmsg,		SY_SETJMP,		/* 102 = 4.2 sendmsg */
    6, sendto,		SY_SETJMP,		/* 103 = 4.2 sendto */
    1, sethostid,	SY_SETJMP,		/* 104 = 4.2 sethostid */
    5, setsockopt,	SY_SETJMP,		/* 105 = 4.2 setsockopt */
    2, shutdown,	SY_SETJMP,		/* 106 = 4.2 shutdown */
    3, socket,		SY_SETJMP,		/* 107 = 4.2 socket */
    2, gethostname,	SY_SETJMP,		/* 108 = 4.2 gethostname */
    2, sethostname,	SY_SETJMP,		/* 109 = 4.2 sethostname */
    2, getdomainname,	SY_SETJMP,		/* 110 = 4.2 getdomainname */
    2, setdomainname,	SY_SETJMP,		/* 111 = 4.2 setdomainname */

    /* other 4.2 compatible system calls */
    2, truncate,	SY_SETJMP,		/* 112 = 4.2 truncate */
    2, ftruncate,	SY_SETJMP,		/* 113 = 4.2 ftruncate */
    2, rename,		SY_SETJMP,		/* 114 = 4.2 rename */
    2, symlink,		SY_SETJMP,		/* 115 = create symbolic link */
    3, readlink,	SY_SETJMP,		/* 116 = read symbolic link */
    2, lstat,		SY_SETJMP,		/* 117 = stat symbolic link */

    /* network filesystem system calls */
    3, nfsmount,	SY_SETJMP,		/* 118 = mount nfs filesystem */
    1, nfs_svc,		SY_SETJMP,		/* 119 = nfs server */
    2, nfs_getfh,	SY_SETJMP,		/* 120 = get file handle */
    0, async_daemon,	SY_SETJMP,		/* 121 = nfs bio daemon */
    3, old_exportfs,	SY_SETJMP,		/* 122 = export served filesys*/

    /* SGI system calls. */
    0, nosys,		0,			/* 123 = SGI - setregid */
    0, nosys,		0,			/* 124 = SGI - setreuid */

    2, getitimer,	SY_SETJMP,		/* 125 = getitimer */
    3, setitimer,	SY_SETJMP,		/* 126 = setitimer */

    2, adjtime,		SY_SETJMP,		/* 127 = adjtime */
    0, nosys,		0,			/* 128 = SGI - bsdgettime */

    0, nosys,		0,			/* 129 = SGI - sproc */
    0, nosys,		0,			/* 130 = SGI - prctl */
    0, nosys,		0,			/* 131 = SGI - blkproc */
    0, nosys,		0,			/* 132 = SGI - not used */

    0, nosys,		0,			/* 133 = SGI - sgigsc */

    0, nosys,		0,			/* 134 = SGI - mmap */
    0, nosys,		0,			/* 135 = SGI - munmap */
    0, nosys,		0,			/* 136 = SGI - mprotect */
    0, nosys,		0,			/* 137 = SGI - msync */
    0, nosys,		0,			/* 138 = SGI - madvise */
    0, nosys,		0,			/* 139 = SGI - mpin, munpin */
    0, nosys,		0,			/* 140 = SGI - getpagesize */

    0, nosys,		0,			/* 141 = SGI - libattach */
    0, nosys,		0,			/* 142 = SGI - libdetach */

    0, nosys,		0,			/* 143 = SGI - BSD getpgrp */
    0, nosys,		0,			/* 144 = SGI - BSD setpgrp */
    0, nosys,		0,			/* 145 = SGI */
    0, nosys,		0,			/* 146 = SGI */
    0, nosys,		0,			/* 147 = SGI */
    0, nosys,		0,			/* 148 = SGI */
    0, nosys,		0,			/* 149 = SGI */

    3, cacheflush,	SY_SETJMP,		/* 150 = cacheflush */
    3, cachectl,	SY_SETJMP,		/* 151 = cachectl */
    3, fchown,		0,			/* 152 = fchown (BSD) */
    2, fchmod,		0,			/* 153 = fchmod (BSD) */
    3, wait3,		SY_SETJMP,		/* 154 = wait, wait2, wait3 */
    6, mmap,		SY_SETJMP,		/* 155 = mmap (BSD) */
    2, munmap,		SY_SETJMP,		/* 156 = munmap (BSD) */
    3, madvise,		SY_SETJMP,		/* 157 = madvise (BSD) */
    1, getpagesize,	0,			/* 158 = getpagesize (BSD) */
    2, setreuid,	SY_SETJMP,		/* 159 = setreuid (BSD) */

    2, setregid,	SY_SETJMP,		/* 160 = setregid (BSD) */
    2, setpgid,		SY_SETJMP,		/* 161 = setpgid (POSIX) */
    2, getgroups,	0,			/* 162 = getgroups (BSD) */
    2, setgroups,	0,			/* 163 = setgroups (BSD) */
    2, gettimeofday,	0,			/* 164 = gettimeofday (BSD) */
    2, getrusage,	0,			/* 165 = getrusage (BSD) */
    2, getrlimit,	0,			/* 166 = getrlimit (BSD) */
    2, setrlimit,	0,			/* 167 = setrlimit (BSD) */
    2, exportfs,	SY_SETJMP,		/* 168 = NFS 4.0 export served fs*/
    3, fcntl,		SY_SETJMP,		/* 169 = fcntl */

    0, nosys,		0,			/* 170 = Rolm Mil-Spec */
    0, nosys,		0,			/* 171 = Rolm Mil-Spec */
    0, nosys,		0,			/* 172 = Rolm Mil-Spec */
    0, nosys,		0,			/* 173 = Rolm Mil-Spec */
    0, nosys,		0,			/* 174 = Rolm Mil-Spec */
    0, nosys,		0,			/* 175 = Rolm Mil-Spec */
    0, nosys,		0,			/* 176 = Rolm Mil-Spec */
    0, nosys,		0,			/* 177 = Rolm Mil-Spec */
    0, nosys,		0,			/* 178 = Rolm Mil-Spec */
    0, nosys,		0,			/* 179 = Rolm Mil-Spec */
    0, nosys,		0,			/* 180 = Rolm Mil-Spec */
    0, nosys,		0,			/* 181 = Rolm Mil-Spec */
    0, nosys,		0,			/* 182 = Rolm Mil-Spec */
    0, nosys,		0,			/* 183 = Rolm Mil-Spec */
    0, nosys,		0,			/* 184 = Rolm Mil-Spec */
    0, nosys,		0,			/* 185 = Rolm Mil-Spec */
    0, nosys,		0,			/* 186 = Rolm Mil-Spec */
    0, nosys,		0,			/* 187 = Rolm Mil-Spec */
    0, nosys,		0,			/* 188 = Rolm Mil-Spec */
    0, nosys,		0,			/* 189 = Rolm Mil-Spec */
    0, nosys,		0,			/* 180 = Rolm Mil-Spec */
    0, nosys,		0,			/* 191 = Rolm Mil-Spec */
    0, nosys,		0,			/* 192 = Rolm Mil-Spec */
    0, nosys,		0,			/* 193 = Rolm Mil-Spec */
    0, nosys,		0,			/* 194 = Rolm Mil-Spec */
    0, nosys,		0,			/* 195 = Rolm Mil-Spec */
    0, nosys,		0,			/* 196 = Rolm Mil-Spec */
    0, nosys,		0,			/* 197 = Rolm Mil-Spec */
    0, nosys,		0,			/* 198 = Rolm Mil-Spec */
    0, nosys,		0,			/* 199 = Rolm Mil-Spec */
    0, nosys,		0,			/* 200 = Rolm Mil-Spec */
    0, nosys,		0,			/* 201 = Rolm Mil-Spec */
    0, nosys,		0,			/* 202 = Rolm Mil-Spec */
    0, nosys,		0,			/* 203 = Rolm Mil-Spec */
    0, nosys,		0,			/* 204 = Rolm Mil-Spec */
    0, nosys,		0,			/* 205 = Rolm Mil-Spec */
    0, nosys,		0,			/* 206 = Rolm Mil-Spec */
    0, nosys,		0,			/* 207 = Rolm Mil-Spec */
    0, nosys,		0,			/* 208 = Rolm Mil-Spec */
    0, nosys,		0,			/* 209 = Rolm Mil-Spec */

    0, nosys,		0,			/* 210 = DDE */
    0, nosys,		0,			/* 211 = DDE */
    0, nosys,		0,			/* 212 = DDE */
    0, nosys,		0,			/* 213 = DDE */
    0, nosys,		0,			/* 214 = DDE */
    0, nosys,		0,			/* 215 = DDE */
    0, nosys,		0,			/* 216 = DDE */
    0, nosys,		0,			/* 217 = DDE */
    0, nosys,		0,			/* 218 = DDE */
    0, nosys,		0,			/* 219 = DDE */
    0, nosys,		0,			/* 220 = DDE */
    0, nosys,		0,			/* 221 = DDE */
    0, nosys,		0,			/* 222 = DDE */
    0, nosys,		0,			/* 223 = DDE */
    0, nosys,		0,			/* 224 = DDE */
    0, nosys,		0,			/* 225 = DDE */
    0, nosys,		0,			/* 226 = DDE */
    0, nosys,		0,			/* 227 = DDE */
    0, nosys,		0,			/* 228 = DDE */
    0, nosys,		0,			/* 229 = DDE */
    0, nosys,		0,			/* 230 = DDE */
    0, nosys,		0,			/* 231 = DDE */
    0, nosys,		0,			/* 232 = DDE */
    0, nosys,		0,			/* 233 = DDE */
    0, nosys,		0,			/* 234 = DDE */
    0, nosys,		0,			/* 235 = DDE */
    0, nosys,		0,			/* 236 = DDE */
    0, nosys,		0,			/* 237 = DDE */
    0, nosys,		0,			/* 238 = DDE */
    0, nosys,		0,			/* 239 = DDE */
    0, nosys,		0,			/* 240 = DDE */
    0, nosys,		0,			/* 241 = DDE */
    0, nosys,		0,			/* 242 = DDE */
    0, nosys,		0,			/* 243 = DDE */
    0, nosys,		0,			/* 244 = DDE */
    0, nosys,		0,			/* 245 = DDE */
    0, nosys,		0,			/* 246 = DDE */
    0, nosys,		0,			/* 247 = DDE */
    0, nosys,		0,			/* 248 = DDE */
    0, nosys,		0,			/* 249 = DDE */
    0, nosys,		0,			/* 250 = DDE */
    0, nosys,		0,			/* 251 = DDE */
    0, nosys,		0,			/* 252 = DDE */
    0, nosys,		0,			/* 253 = DDE */
    0, nosys,		0,			/* 254 = DDE */
    0, nosys,		0,			/* 255 = DDE */
    0, nosys,		0,			/* 256 = DDE */
    0, nosys,		0,			/* 257 = DDE */
    0, nosys,		0,			/* 258 = DDE */
    0, nosys,		0,			/* 259 = DDE */

    0, nosys,		0,			/* 260 = future netboot */
    0, nosys,		0,			/* 261 = future netunboot */
    0, nosys,		0,			/* 262 = future rdump */
    3, libattach,	SY_SETJMP,		/* 263 = libattach */
    1, libdetach,	SY_SETJMP,		/* 264 = libdetach */
    0, nosys,		0,			/* 265 = MIPS */
    0, nosys,		0,			/* 266 = MIPS */
    0, nosys,		0,			/* 267 = MIPS */
    0, nosys,		0,			/* 268 = MIPS */
    0, nosys,		0,			/* 269 = MIPS */
    0, nosys,		0,			/* 270 = MIPS */
    0, nosys,		0,			/* 271 = MIPS */
    0, nosys,		0,			/* 272 = MIPS */
    0, nosys,		0,			/* 273 = MIPS */
    0, nosys,		0,			/* 274 = MIPS */
    0, nosys,		0,			/* 275 = MIPS */
    0, nosys,		0,			/* 276 = MIPS */
    0, nosys,		0,			/* 277 = MIPS */
    0, nosys,		0,			/* 278 = MIPS */
    0, nosys,		0,			/* 279 = MIPS */

    /* Additional Reservations: 
     *		280..299	Tandem
     *		300..319	SGI
     *		320..339	University of Sydney
     *		340..359	Concurrent
     *		360..409	CDC
     *		410..699	(unreserved)
     *		700..829	Concurrent
     *		830..999	(unreserved)
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
     *  Currently, reserved sysno's are marked MIPS, SGI,
     *  Rolm Mil-Spec, DDE, Tandem, U. of Sydney, or Concurrent.
     */
};

int nsysent = sizeof(sysent)/sizeof(sysent[0]);
