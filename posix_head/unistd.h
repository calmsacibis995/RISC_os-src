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
/* $Header: unistd.h,v 1.3.1.5 90/05/10 04:10:36 wje Exp $ */

#ifndef	_POSIX_UNISTD_
#define	_POSIX_UNISTD_	1

#include <sysv/unistd.h>
#include <sys/types.h>

#ifndef	NULL
#define NULL 	0
#endif

#ifndef	CLK_TCK
#define CLK_TCK	100
#endif 

#ifndef _SSIZE_T
#define _SSIZE_T
typedef	int		ssize_t;
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned int	size_t;
#endif

/* File descriptors for stdin, stdout and stderr */
#define STDIN_FILENO    0
#define STDOUT_FILENO   1
#define STDERR_FILENO   2

/* Compile-time symbolic constants -- can be interrogated by sysconf() */
#define _POSIX_JOB_CONTROL	1 /* implementation supports job control */
#define _POSIX_SAVED_IDS	1 /* each process has a saved set-user-ID */
				  /* and saved set-group-ID		  */
#define _POSIX_VERSION 		198808L /* version */

/* Execution-time symbolic constants for portability specifications --
 * may be interrogated using pathconf()
 */
/* chown function is restricted */
#undef	_POSIX_CHOWN_RESTRICTED 	/* configurable -- must query */
/* Path components greater than NAME_MAX are never truncated */
#define _POSIX_NO_TRUNC 	1 
/* value to use for disabling special character functions */
#undef 	_POSIX_VDISABLE 		/* configurable -- must query */

/* DEFINES for sysconf() options */
#define _SC_ARG_MAX 	1
#define _SC_CHILD_MAX 	2
#define _SC_CLK_TCK 	3
#define _SC_NGROUPS_MAX 4
#define _SC_OPEN_MAX 	5
#define _SC_JOB_CONTROL 6
#define _SC_SAVED_IDS 	7
#define _SC_VERSION 	8
#define _SC_STREAM_MAX 	9
#define _SC_TZNAME_MAX 	10

/* DEFINES for pathconf() options */
#define _PC_LINK_MAX 1
#define _PC_MAX_CANON 2
#define _PC_MAX_INPUT 3
#define _PC_NAME_MAX 4
#define _PC_PATH_MAX 5
#define _PC_PIPE_BUF 6
#define _PC_CHOWN_RESTRICTED 7
#define _PC_NO_TRUNC 8
#define _PC_VDISABLE 9

extern pid_t fork();
extern int execl(), execle(), execlp(), execv(), execve(), execvp(); 
extern void exit(), _exit();
extern unsigned alarm();
extern unsigned sleep();
extern int pause();
extern pid_t getpid(), getppid();
extern uid_t geteuid(), getuid();
extern gid_t getgid(), getegid();
extern int setuid(), setgid();
extern int getgroups();
extern char *getlogin();
extern pid_t getpgrp();
extern pid_t setsid();
extern pid_t setpgid();
extern char *getenv();
extern char *ttyname();
extern int isatty();
extern long sysconf();
extern int chdir();
extern char *getcwd();
extern int link(), unlink();
extern int rmdir();
extern int rename();
extern int access();
extern int chown();
extern long pathconf(), fpathconf();
extern int pipe();
extern int dup(), dup2();
extern int close();
extern int read();
extern int write();
extern off_t lseek();
extern pid_t tcgetpgrp();
extern int tcsetpgrp();

#endif	_POSIX_UNISTD_
