/* $Header: syscall.h,v 1.9.1.4.1.2 90/07/11 18:27:35 hawkes Exp $ */
/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * syscall.h -- system call numbers
 *
 * NOTE for NUMIPS:
 *
 *	This file must match the kernel file "bsd43_sysent.c".
 *
 *	Each NUMIPS system gets its own range of system call numbers.
 *	The range for BSD compatibility is 2000-2999.
 */

#define	BSD43_SYS_syscall	2000
#define	BSD43_SYS_exit		2001
#define	BSD43_SYS_fork		2002
#define	BSD43_SYS_read		2003
#define	BSD43_SYS_write		2004
#define	BSD43_SYS_open		2005
#define	BSD43_SYS_close		2006
					/*  7 is old: wait */
#define	BSD43_SYS_creat		2008
#define	BSD43_SYS_link		2009
#define	BSD43_SYS_unlink	2010
#define	BSD43_SYS_execv		2011
#define	BSD43_SYS_chdir		2012
					/* 13 is old: time */
#define	BSD43_SYS_mknod		2014
#define	BSD43_SYS_chmod		2015
#define	BSD43_SYS_chown		2016
#define	BSD43_SYS_brk		2017	/* 17 is old: sbreak */
					/* 18 is old: stat */
#define	BSD43_SYS_lseek		2019
#define	BSD43_SYS_getpid	2020
#define	BSD43_SYS_omount	2021	/* 21 is old: mount */
#define	BSD43_SYS_oumount	2022	/* 22 is old: umount */
					/* 23 is old: setuid */
#define	BSD43_SYS_getuid	2024
					/* 25 is old: stime */
#define	BSD43_SYS_ptrace	2026
					/* 27 is old: alarm */
					/* 28 is old: fstat */
					/* 29 is old: pause */
					/* 30 is old: utime */
					/* 31 is old: stty */
					/* 32 is old: gtty */
#define	BSD43_SYS_access	2033
					/* 34 is old: nice */
					/* 35 is old: ftime */
#define	BSD43_SYS_sync		2036
#define	BSD43_SYS_kill		2037
#define	BSD43_SYS_stat		2038
					/* 39 is old: setpgrp */
#define	BSD43_SYS_lstat		2040
#define	BSD43_SYS_dup		2041
#define	BSD43_SYS_pipe		2042
					/* 43 is old: times */
#define	BSD43_SYS_profil	2044
					/* 45 is unused */
					/* 46 is old: setgid */
#define	BSD43_SYS_getgid	2047
					/* 48 is old: sigsys */
					/* 49 is unused */
					/* 50 is unused */
#define	BSD43_SYS_acct		2051
					/* 52 is old: phys */
					/* 53 is old: syslock */
#define	BSD43_SYS_ioctl		2054
#define	BSD43_SYS_reboot	2055
					/* 56 is old: mpxchan */
#define	BSD43_SYS_symlink	2057
#define	BSD43_SYS_readlink	2058
#define	BSD43_SYS_execve	2059
#define	BSD43_SYS_umask		2060
#define	BSD43_SYS_chroot	2061
#define	BSD43_SYS_fstat		2062
					/* 63 is unused */
#define	BSD43_SYS_getpagesize 	2064
#define	BSD43_SYS_mremap	2065
#define	BSD43_SYS_vfork		2066
					/* 67 is old: vread */
					/* 68 is old: vwrite */
#define	BSD43_SYS_sbrk		2069
#define	BSD43_SYS_sstk		2070
#define	BSD43_SYS_mmap		2071
#define	BSD43_SYS_vadvise	2072	/* 72 is old: vadvise */
#define	BSD43_SYS_munmap	2073
#define	BSD43_SYS_mprotec	2074
#define	BSD43_SYS_madvise	2075
#define	BSD43_SYS_vhangup	2076
					/* 77 is old: vlimit */
#define	BSD43_SYS_mincore	2078
#define	BSD43_SYS_getgroups	2079
#define	BSD43_SYS_setgroups	2080
#define	BSD43_SYS_getpgrp	2081
#define	BSD43_SYS_setpgrp	2082
#define	BSD43_SYS_setitimer	2083
#define	BSD43_SYS_wait3		2084
#define	BSD43_SYS_swapon	2085
#define	BSD43_SYS_getitimer	2086
#define	BSD43_SYS_gethostname	2087
#define	BSD43_SYS_sethostname	2088
#define	BSD43_SYS_getdtablesize	2089
#define	BSD43_SYS_dup2		2090
#define	BSD43_SYS_getdopt	2091
#define	BSD43_SYS_fcntl		2092
#define	BSD43_SYS_select	2093
#define	BSD43_SYS_setdopt	2094
#define	BSD43_SYS_fsync		2095
#define	BSD43_SYS_setpriority	2096
#define	BSD43_SYS_socket	2097
#define	BSD43_SYS_connect	2098
#define	BSD43_SYS_accept	2099
#define	BSD43_SYS_getpriority	2100
#define	BSD43_SYS_send		2101
#define	BSD43_SYS_recv		2102
#define	BSD43_SYS_sigreturn	2103
#define	BSD43_SYS_bind		2104
#define	BSD43_SYS_setsockopt	2105
#define	BSD43_SYS_listen	2106
					/* 107 was vtimes */
#define	BSD43_SYS_sigvec	2108
#define	BSD43_SYS_sigblock	2109
#define	BSD43_SYS_sigsetmask	2110
#define	BSD43_SYS_sigpause	2111
#define	BSD43_SYS_sigstack	2112
#define	BSD43_SYS_recvmsg	2113
#define	BSD43_SYS_sendmsg	2114
					/* 115 is old vtrace */
#define	BSD43_SYS_gettimeofday	2116
#define	BSD43_SYS_getrusage	2117
#define	BSD43_SYS_getsockopt	2118
					/* 119 is old resuba */
#define	BSD43_SYS_readv		2120
#define	BSD43_SYS_writev	2121
#define	BSD43_SYS_settimeofday	2122
#define	BSD43_SYS_fchown	2123
#define	BSD43_SYS_fchmod	2124
#define	BSD43_SYS_recvfrom	2125
#define	BSD43_SYS_setreuid	2126
#define	BSD43_SYS_setregid	2127
#define	BSD43_SYS_rename	2128
#define	BSD43_SYS_truncate	2129
#define	BSD43_SYS_ftruncate	2130
#define	BSD43_SYS_flock		2131
					/* 132 is unused */
#define	BSD43_SYS_sendto	2133
#define	BSD43_SYS_shutdown	2134
#define	BSD43_SYS_socketpair	2135
#define	BSD43_SYS_mkdir		2136
#define	BSD43_SYS_rmdir		2137
#define	BSD43_SYS_utimes	2138
#define	BSD43_SYS_sigcleanup	2139	/* From 4.2 longjmp; same as SYS_sigreturn */
#define	BSD43_SYS_adjtime	2140
#define	BSD43_SYS_getpeername	2141
#define	BSD43_SYS_gethostid	2142
#define	BSD43_SYS_sethostid	2143
#define	BSD43_SYS_getrlimit	2144
#define	BSD43_SYS_setrlimit	2145
#define	BSD43_SYS_killpg	2146
					/* 147 is unused */
#define	BSD43_SYS_setquota	2148	/* 148 is old: */
#define	BSD43_SYS_quota		2149	/* 149 is old: */
#define	BSD43_SYS_getsockname	2150

/*
 * mips local system calls
 */
#define	BSD43_SYS_sysmips	2151
#define	BSD43_SYS_cacheflush	2152
#define	BSD43_SYS_cachectl	2153

/*
 * nfs releated system calls
 */
#define BSD43_SYS_debug		2154
					/* 155 is unused */
					/* 156 is unused */
					/* 157 is old: nfs_mount */
#define BSD43_SYS_nfssvc	2158
#define BSD43_SYS_getdirentries	2159
#define BSD43_SYS_statfs	2160
#define BSD43_SYS_fstatfs	2161
#define BSD43_SYS_unmount	2162
#define BSD43_SYS_async_daemon	2163
#define BSD43_SYS_getfh		2164
#define BSD43_SYS_getdomainname	2165
#define BSD43_SYS_setdomainname	2166
					/* 167 is old: pcfs_mount */
#define BSD43_SYS_quotactl	2168
#define BSD43_SYS_old_exportfs	2169	/* 169 is old: kept for bin compat */
#define BSD43_SYS_mount		2170

/*
 * More random system calls
 */
#define	BSD43_SYS_hdwconf	2171	/* mips local system call */
#define BSD43_SYS_exportfs	2172	/* NFS4.0 version */
#define BSD43_SYS_nfsfh_open    2173	/* NFS4.0 external file handle open */

/*
 * system calls to support MNLS.
 */
#define BSD43_SYS_libattach     2174	/* libattach*/
#define BSD43_SYS_libdetach     2175	/* libdetach */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define SYS_accept BSD43_SYS_accept
#   define SYS_access BSD43_SYS_access
#   define SYS_acct BSD43_SYS_acct
#   define SYS_adjtime BSD43_SYS_adjtime
#   define SYS_async_daemon BSD43_SYS_async_daemon
#   define SYS_bind BSD43_SYS_bind
#   define SYS_brk BSD43_SYS_brk
#   define SYS_cachectl BSD43_SYS_cachectl
#   define SYS_cacheflush BSD43_SYS_cacheflush
#   define SYS_chdir BSD43_SYS_chdir
#   define SYS_chmod BSD43_SYS_chmod
#   define SYS_chown BSD43_SYS_chown
#   define SYS_chroot BSD43_SYS_chroot
#   define SYS_close BSD43_SYS_close
#   define SYS_connect BSD43_SYS_connect
#   define SYS_creat BSD43_SYS_creat
#   define SYS_debug BSD43_SYS_debug
#   define SYS_dup BSD43_SYS_dup
#   define SYS_dup2 BSD43_SYS_dup2
#   define SYS_execv BSD43_SYS_execv
#   define SYS_execve BSD43_SYS_execve
#   define SYS_exit BSD43_SYS_exit
#   define SYS_exportfs BSD43_SYS_exportfs
#   define SYS_fchmod BSD43_SYS_fchmod
#   define SYS_fchown BSD43_SYS_fchown
#   define SYS_fcntl BSD43_SYS_fcntl
#   define SYS_flock BSD43_SYS_flock
#   define SYS_fork BSD43_SYS_fork
#   define SYS_fstat BSD43_SYS_fstat
#   define SYS_fstatfs BSD43_SYS_fstatfs
#   define SYS_fsync BSD43_SYS_fsync
#   define SYS_ftruncate BSD43_SYS_ftruncate
#   define SYS_getdirentries BSD43_SYS_getdirentries
#   define SYS_getdomainname BSD43_SYS_getdomainname
#   define SYS_getdopt BSD43_SYS_getdopt
#   define SYS_getdtablesize BSD43_SYS_getdtablesize
#   define SYS_getfh BSD43_SYS_getfh
#   define SYS_getgid BSD43_SYS_getgid
#   define SYS_getgroups BSD43_SYS_getgroups
#   define SYS_gethostid BSD43_SYS_gethostid
#   define SYS_gethostname BSD43_SYS_gethostname
#   define SYS_getitimer BSD43_SYS_getitimer
#   define SYS_getpagesize BSD43_SYS_getpagesize
#   define SYS_getpeername BSD43_SYS_getpeername
#   define SYS_getpgrp BSD43_SYS_getpgrp
#   define SYS_getpid BSD43_SYS_getpid
#   define SYS_getpriority BSD43_SYS_getpriority
#   define SYS_getrlimit BSD43_SYS_getrlimit
#   define SYS_getrusage BSD43_SYS_getrusage
#   define SYS_getsockname BSD43_SYS_getsockname
#   define SYS_getsockopt BSD43_SYS_getsockopt
#   define SYS_gettimeofday BSD43_SYS_gettimeofday
#   define SYS_getuid BSD43_SYS_getuid
#   define SYS_hdwconf BSD43_SYS_hdwconf
#   define SYS_ioctl BSD43_SYS_ioctl
#   define SYS_kill BSD43_SYS_kill
#   define SYS_killpg BSD43_SYS_killpg
#   define SYS_libattach BSD43_SYS_libattach
#   define SYS_libdetach BSD43_SYS_libdetach
#   define SYS_link BSD43_SYS_link
#   define SYS_listen BSD43_SYS_listen
#   define SYS_lseek BSD43_SYS_lseek
#   define SYS_lstat BSD43_SYS_lstat
#   define SYS_madvise BSD43_SYS_madvise
#   define SYS_mincore BSD43_SYS_mincore
#   define SYS_mkdir BSD43_SYS_mkdir
#   define SYS_mknod BSD43_SYS_mknod
#   define SYS_mmap BSD43_SYS_mmap
#   define SYS_mount BSD43_SYS_mount
#   define SYS_mprotect BSD43_SYS_mprotect
#   define SYS_mremap BSD43_SYS_mremap
#   define SYS_munmap BSD43_SYS_munmap
#   define SYS_nfsfh_open BSD43_SYS_nfsfh_open
#   define SYS_nfssvc BSD43_SYS_nfssvc
#   define SYS_old_exportfs BSD43_SYS_old_exportfs
#   define SYS_omount BSD43_SYS_omount
#   define SYS_open BSD43_SYS_open
#   define SYS_oumount BSD43_SYS_oumount
#   define SYS_pipe BSD43_SYS_pipe
#   define SYS_profil BSD43_SYS_profil
#   define SYS_ptrace BSD43_SYS_ptrace
#   define SYS_quota BSD43_SYS_quota
#   define SYS_quotactl BSD43_SYS_quotactl
#   define SYS_read BSD43_SYS_read
#   define SYS_readlink BSD43_SYS_readlink
#   define SYS_readv BSD43_SYS_readv
#   define SYS_reboot BSD43_SYS_reboot
#   define SYS_recv BSD43_SYS_recv
#   define SYS_recvfrom BSD43_SYS_recvfrom
#   define SYS_recvmsg BSD43_SYS_recvmsg
#   define SYS_rename BSD43_SYS_rename
#   define SYS_rmdir BSD43_SYS_rmdir
#   define SYS_sbrk BSD43_SYS_sbrk
#   define SYS_select BSD43_SYS_select
#   define SYS_send BSD43_SYS_send
#   define SYS_sendmsg BSD43_SYS_sendmsg
#   define SYS_sendto BSD43_SYS_sendto
#   define SYS_setdomainname BSD43_SYS_setdomainname
#   define SYS_setdopt BSD43_SYS_setdopt
#   define SYS_setgroups BSD43_SYS_setgroups
#   define SYS_sethostid BSD43_SYS_sethostid
#   define SYS_sethostname BSD43_SYS_sethostname
#   define SYS_setitimer BSD43_SYS_setitimer
#   define SYS_setpgrp BSD43_SYS_setpgrp
#   define SYS_setpriority BSD43_SYS_setpriority
#   define SYS_setquota BSD43_SYS_setquota
#   define SYS_setregid BSD43_SYS_setregid
#   define SYS_setreuid BSD43_SYS_setreuid
#   define SYS_setrlimit BSD43_SYS_setrlimit
#   define SYS_setsockopt BSD43_SYS_setsockopt
#   define SYS_settimeofday BSD43_SYS_settimeofday
#   define SYS_shutdown BSD43_SYS_shutdown
#   define SYS_sigblock BSD43_SYS_sigblock
#   define SYS_sigcleanup BSD43_SYS_sigcleanup
#   define SYS_sigpause BSD43_SYS_sigpause
#   define SYS_sigreturn BSD43_SYS_sigreturn
#   define SYS_sigsetmask BSD43_SYS_sigsetmask
#   define SYS_sigstack BSD43_SYS_sigstack
#   define SYS_sigvec BSD43_SYS_sigvec
#   define SYS_socket BSD43_SYS_socket
#   define SYS_socketpair BSD43_SYS_socketpair
#   define SYS_sstk BSD43_SYS_sstk
#   define SYS_stat BSD43_SYS_stat
#   define SYS_statfs BSD43_SYS_statfs
#   define SYS_swapon BSD43_SYS_swapon
#   define SYS_symlink BSD43_SYS_symlink
#   define SYS_sync BSD43_SYS_sync
#   define SYS_syscall BSD43_SYS_syscall
#   define SYS_sysmips BSD43_SYS_sysmips
#   define SYS_truncate BSD43_SYS_truncate
#   define SYS_umask BSD43_SYS_umask
#   define SYS_unlink BSD43_SYS_unlink
#   define SYS_unmount BSD43_SYS_unmount
#   define SYS_utimes BSD43_SYS_utimes
#   define SYS_vadvise BSD43_SYS_vadvise
#   define SYS_vfork BSD43_SYS_vfork
#   define SYS_vhangup BSD43_SYS_vhangup
#   define SYS_wait3 BSD43_SYS_wait3
#   define SYS_write BSD43_SYS_write
#   define SYS_writev BSD43_SYS_writev
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


