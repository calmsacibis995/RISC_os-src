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
/* $Header: sys.s,v 1.3.1.2 90/05/10 04:09:42 wje Exp $ */
/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 *
 * $Header: sys.s,v 1.3.1.2 90/05/10 04:09:42 wje Exp $
 */

/* POSIX System call numbers */
/* These must match with the table entries appearing in 
 * posix/glues/posix_sysent.c 
 */

#define POSIXoffset	3000	

#define SYS_syscall	(0+POSIXoffset)
#define SYS_exit	(1+POSIXoffset)
#define SYS_fork	(2+POSIXoffset)
#define SYS_read	(3+POSIXoffset)
#define SYS_write	(4+POSIXoffset)
#define SYS_open	(5+POSIXoffset)
#define SYS_close	(6+POSIXoffset)
#define SYS_wait	(7+POSIXoffset)
#define SYS_creat	(8+POSIXoffset)
#define SYS_link	(9+POSIXoffset)
#define SYS_unlink	(10+POSIXoffset)
#define SYS_execv	(11+POSIXoffset)
#define SYS_chdir	(12+POSIXoffset)
#define SYS_time	(13+POSIXoffset)
#define SYS_mknod	(14+POSIXoffset)
#define SYS_chmod	(15+POSIXoffset)
#define SYS_chown	(16+POSIXoffset)
#define SYS_brk		(17+POSIXoffset)
#define SYS_stat	(18+POSIXoffset)
#define SYS_lseek	(19+POSIXoffset)
#define SYS_getpid	(20+POSIXoffset)
#define SYS_mount	(21+POSIXoffset)
#define SYS_umount	(22+POSIXoffset)
#define SYS_setuid	(23+POSIXoffset)
#define SYS_getuid	(24+POSIXoffset)
#define SYS_stime	(25+POSIXoffset)
#define SYS_ptrace	(26+POSIXoffset)
#define SYS_alarm	(27+POSIXoffset)
#define SYS_fstat	(28+POSIXoffset)
#define SYS_pause	(29+POSIXoffset)
#define SYS_utime	(30+POSIXoffset)
#define SYS_stty	(31+POSIXoffset)
#define SYS_gtty	(32+POSIXoffset)
#define SYS_access	(33+POSIXoffset)
#define SYS_nice	(34+POSIXoffset)
#define SYS_statfs	(35+POSIXoffset)
#define SYS_sync	(36+POSIXoffset)
#define SYS_kill	(37+POSIXoffset)
#define SYS_fstatfs	(38+POSIXoffset)
#define SYS_getpgrp	(39+POSIXoffset)
#define SYS_dup		(41+POSIXoffset)
#define SYS_pipe	(42+POSIXoffset)
#define SYS_times	(43+POSIXoffset)
#define SYS_profil	(44+POSIXoffset)
#define SYS_plock	(45+POSIXoffset)
#define SYS_setgid	(46+POSIXoffset)
#define SYS_getgid	(47+POSIXoffset)
#define SYS_signal	(48+POSIXoffset)
#define SYS_msgsys	(49+POSIXoffset)
#define SYS_sysmips	(50+POSIXoffset)
#define SYS_acct	(51+POSIXoffset)
#define SYS_shmsys	(52+POSIXoffset)
#define SYS_semsys	(53+POSIXoffset)
#define SYS_ioctl	(54+POSIXoffset)
#define SYS_uadmin	(55+POSIXoffset)
#define SYS_utssys	(57+POSIXoffset)
#define SYS_execve	(59+POSIXoffset)
#define SYS_umask	(60+POSIXoffset)
#define SYS_chroot	(61+POSIXoffset)
#define SYS_fcntl	(62+POSIXoffset)
#define SYS_ulimit	(63+POSIXoffset)
#define SYS_advfs	(70+POSIXoffset)
#define SYS_unadvfs	(71+POSIXoffset)
#define SYS_rmount	(72+POSIXoffset)
#define SYS_rumount	(73+POSIXoffset)
#define SYS_rfstart	(74+POSIXoffset)
#define SYS_rdebug	(76+POSIXoffset)
#define SYS_rfstop	(77+POSIXoffset)
#define SYS_rfsys	(78+POSIXoffset)
#define SYS_rmdir	(79+POSIXoffset)
#define SYS_mkdir	(80+POSIXoffset)
#define SYS__getdents	(81+POSIXoffset)
#define SYS_sysfs	(84+POSIXoffset)
#define SYS_getmsg	(85+POSIXoffset)
#define SYS_putmsg	(86+POSIXoffset)
#define SYS_poll	(87+POSIXoffset)
/* This system call is internal for mips signal handling code */
#define SYS_sigreturn	(88+POSIXoffset)

/* 4.2 compatible socket/TCP/IP system calls */
#define SYS_accept	(89+POSIXoffset)
#define SYS_bind	(90+POSIXoffset)
#define SYS_connect	(91+POSIXoffset)
#define SYS_gethostid	(92+POSIXoffset)
#define SYS_getpeername	(93+POSIXoffset)
#define SYS_getsockname	(94+POSIXoffset)
#define SYS_getsockopt	(95+POSIXoffset)
#define SYS_listen	(96+POSIXoffset)
#define SYS_recv	(97+POSIXoffset)
#define SYS_recvfrom	(98+POSIXoffset)
#define SYS_recvmsg	(99+POSIXoffset)
#define SYS_select	(100+POSIXoffset)
#define SYS_send	(101+POSIXoffset)
#define SYS_sendmsg	(102+POSIXoffset)
#define SYS_sendto	(103+POSIXoffset)
#define SYS_sethostid	(104+POSIXoffset)
#define SYS_setsockopt	(105+POSIXoffset)
#define SYS_shutdown	(106+POSIXoffset)
#define SYS_socket	(107+POSIXoffset)
#define SYS_gethostname	(108+POSIXoffset)
#define SYS_sethostname	(109+POSIXoffset)
#define SYS_getdomainname (110+POSIXoffset)
#define SYS_setdomainname (111+POSIXoffset)

/* other 4.2 system calls */
#define SYS_truncate	(112+POSIXoffset)
#define SYS_ftruncate	(113+POSIXoffset)
#define SYS_rename	(114+POSIXoffset)
#define	SYS_symlink	(115+POSIXoffset)
#define	SYS_readlink	(116+POSIXoffset)
#define	SYS_lstat	(117+POSIXoffset)
#define	SYS_nfsmount	(118+POSIXoffset)
#define	SYS_nfssvc	(119+POSIXoffset)
#define	SYS_getfh	(120+POSIXoffset)
#define	SYS_async_daemon (121+POSIXoffset)
#define	SYS_exportfs	(122+POSIXoffset)

#if 0
/*
 * These interfaces are left for backward compatability, but are
 * no longer used.
 */

#define	SYS_mmap	(123+POSIXoffset)
#define	SYS_munmap	(124+POSIXoffset)
#endif

#define	SYS_getitimer	(125+POSIXoffset)
#define	SYS_setitimer	(126+POSIXoffset)

/* more system calls */
#define	SYS_cacheflush	(150+POSIXoffset)
#define	SYS_cachectl	(151+POSIXoffset)
#define SYS_fchown	(152+POSIXoffset)
#define SYS_fchmod	(153+POSIXoffset)
#define SYS_wait3	(154+POSIXoffset)
#define SYS_mmap	(155+POSIXoffset)
#define SYS_munmap	(156+POSIXoffset)
#define SYS_madvise	(157+POSIXoffset)
#define SYS__getpagesize	(158+POSIXoffset)
#define SYS_setreuid	(159+POSIXoffset)
#define SYS_setregid	(160+POSIXoffset)
#define SYS_setpgid	(161+POSIXoffset)
#define SYS_getgroups	(162+POSIXoffset)
#define SYS_setgroups	(163+POSIXoffset)
#define SYS_gettimeofday (164+POSIXoffset)
#define SYS_getrusage	(165+POSIXoffset)
#define SYS_getrlimit	(166+POSIXoffset)
#define SYS_setrlimit	(167+POSIXoffset)
#define SYS_waitpid	(168+POSIXoffset)
#define SYS_dup2	(169+POSIXoffset)


/* These calls have library stubs but no kernel handler */
/* Either AT+T will have to provide a handler later or we will have to
   nuke them */
#define SYS_netboot	(260+POSIXoffset)
#define SYS_netunboot	(261+POSIXoffset)
#define SYS_rdump	(262+POSIXoffset)

/* New POSIX system calls */
#define SYS_setsid	(263+POSIXoffset)
#define SYS___getmaxsig	(264+POSIXoffset)
#define SYS_sigpending	(265+POSIXoffset)
#define SYS_sigprocmask	(266+POSIXoffset)
#define SYS_sigsuspend	(267+POSIXoffset)
#define SYS_sigaction	(268+POSIXoffset)
