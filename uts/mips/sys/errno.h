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
/* $Header: errno.h,v 1.11.2.6 90/05/10 06:12:40 wje Exp $ */

#ifndef	_SYS_ERRNO_
#define	_SYS_ERRNO_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Error codes
 */

#define	EPERM	1	/* Not super-user			*/
#define	ENOENT	2	/* No such file or directory		*/
#define	ESRCH	3	/* No such process			*/
#define	EINTR	4	/* interrupted system call		*/
#define	EIO	5	/* I/O error				*/
#define	ENXIO	6	/* No such device or address		*/
#define	E2BIG	7	/* Arg list too long			*/
#define	ENOEXEC	8	/* Exec format error			*/
#define	EBADF	9	/* Bad file number			*/
#define	ECHILD	10	/* No children				*/
#define	EAGAIN	11	/* No more processes			*/
#define	ENOMEM	12	/* Not enough core			*/
#define	EACCES	13	/* Permission denied			*/
#define	EFAULT	14	/* Bad address				*/
#define	ENOTBLK	15	/* Block device required		*/
#define	EBUSY	16	/* Mount device busy			*/
#define	EEXIST	17	/* File exists				*/
#define	EXDEV	18	/* Cross-device link			*/
#define	ENODEV	19	/* No such device			*/
#define	ENOTDIR	20	/* Not a directory			*/
#define	EISDIR	21	/* Is a directory			*/
#define	EINVAL	22	/* Invalid argument			*/
#define	ENFILE	23	/* File table overflow			*/
#define	EMFILE	24	/* Too many open files			*/
#define	ENOTTY	25	/* Not a typewriter			*/
#define	ETXTBSY	26	/* Text file busy			*/
#define	EFBIG	27	/* File too large			*/
#define	ENOSPC	28	/* No space left on device		*/
#define	ESPIPE	29	/* Illegal seek				*/
#define	EROFS	30	/* Read only file system		*/
#define	EMLINK	31	/* Too many links			*/
#define	EPIPE	32	/* Broken pipe				*/
#define	EDOM	33	/* Math arg out of domain of func	*/
#define	ERANGE	34	/* Math result not representable	*/
#define	ENOMSG	35	/* No message of desired type		*/
#define	EIDRM	36	/* Identifier removed			*/
#define	ECHRNG	37	/* Channel number out of range		*/
#define	EL2NSYNC 38	/* Level 2 not synchronized		*/
#define	EL3HLT	39	/* Level 3 halted			*/
#define	EL3RST	40	/* Level 3 reset			*/
#define	ELNRNG	41	/* Link number out of range		*/
#define	EUNATCH 42	/* Protocol driver not attached		*/
#define	ENOCSI	43	/* No CSI structure available		*/
#define	EL2HLT	44	/* Level 2 halted			*/
#define	EDEADLK	45	/* Deadlock condition.			*/
#define	ENOLCK	46	/* No record locks available.		*/

/* Convergent Error Returns */
#define EBADE	50	/* invalid exchange			*/
#define EBADR	51	/* invalid request descriptor		*/
#define EXFULL	52	/* exchange full			*/
#define ENOANO	53	/* no anode				*/
#define EBADRQC	54	/* invalid request code			*/
#define EBADSLT	55	/* invalid slot				*/
#define EDEADLOCK 56	/* file locking deadlock error		*/

#define EBFONT	57	/* bad font file fmt			*/

/* stream problems */
#define ENOSTR	60	/* Device not a stream			*/
#define ENODATA	61	/* no data (for no delay io)		*/
#define ETIME	62	/* timer expired			*/
#define ENOSR	63	/* out of streams resources		*/

#define ENONET	64	/* Machine is not on the network	*/
#define ENOPKG	65	/* Package not installed                */
#define EREMOTE	66	/* The object is remote			*/
#define ENOLINK	67	/* the link has been severed */
#define EADV	68	/* advertise error */
#define ESRMNT	69	/* srmount error */

#define	ECOMM	70	/* Communication error on send		*/
#define EPROTO	71	/* Protocol error			*/
#define	EMULTIHOP 74	/* multihop attempted */
#define	ELBIN	75	/* Inode is remote (not really error)*/
#define	EDOTDOT 76	/* Cross mount point (not really error)*/
#define EBADMSG 77	/* trying to read unreadable message	*/

#define ENOTUNIQ 80	/* given log. name not unique */
#define EBADFD	 81	/* f.d. invalid for this operation */
#define EREMCHG	 82	/* Remote address changed */

/* shared library problems */
#define ELIBACC	83	/* Can't access a needed shared lib.	*/
#define ELIBBAD	84	/* Accessing a corrupted shared lib.	*/
#define ELIBSCN	85	/* .lib section in a.out corrupted.	*/
#define ELIBMAX	86	/* Attempting to link in too many libs.	*/
#define ELIBEXEC 87	/* Attempting to exec a shared library.	*/

#define	EPROCLIM 88	/* Too many processes 			*/
#define	EUSERS	89	/* Too many users 			*/
#define ENOTSUP 90	/* Not supported.			*/

/* required by POSIX for completeness */
#define ENOSYS 99	/* Function not implemented		*/
/*
 * networking error numbers (tcp/ip)
 */
#define TCP_EBASE 100
/* non-blocking and interrupt i/o */
#define EWOULDBLOCK	(1+TCP_EBASE)	/* Operation would block */
#define EINPROGRESS	(2+TCP_EBASE)	/* Operation now in progress */
#define EALREADY	(3+TCP_EBASE)	/* Operation already in progress */

/* argument errors */
#define ENOTSOCK	(4+TCP_EBASE)	/* Socket operation on non-socket */
#define EDESTADDRREQ	(5+TCP_EBASE)	/* Destination address required */
#define EMSGSIZE	(6+TCP_EBASE)	/* Message too long */
#define EPROTOTYPE	(7+TCP_EBASE)	/* Protocol wrong type for socket */
#define ENOPROTOOPT	(8+TCP_EBASE)	/* Protocol not available */
#define EPROTONOSUPPORT	(9+TCP_EBASE)	/* Protocol not supported */
#define ESOCKTNOSUPPORT	(10+TCP_EBASE)	/* Socket type not supported */
#define EOPNOTSUPP	(11+TCP_EBASE)	/* Op not supported on socket */
#define EPFNOSUPPORT	(12+TCP_EBASE)	/* Protocol family not supported */
#define EAFNOSUPPORT	(13+TCP_EBASE)	/* Address family not supported by protocol family */
#define EADDRINUSE	(14+TCP_EBASE)	/* Address already in use */
#define EADDRNOTAVAIL	(15+TCP_EBASE)	/* Can't assign requested address */

/* operational errors */
#define ENETDOWN	(16+TCP_EBASE)	/* Network is down */
#define ENETUNREACH	(17+TCP_EBASE)	/* Network is unreachable */
#define ENETRESET	(18+TCP_EBASE)	/* Network dropped connection on reset */
#define ECONNABORTED	(19+TCP_EBASE)	/* Software caused connection abort */
#define ECONNRESET	(20+TCP_EBASE)	/* Connection reset by peer */
#define ENOBUFS 	(21+TCP_EBASE)	/* No buffer space available */
#define EISCONN 	(22+TCP_EBASE)	/* Socket is already connected */
#define ENOTCONN	(23+TCP_EBASE)	/* Socket is not connected */
#define ESHUTDOWN	(24+TCP_EBASE)	/* Can't send after socket shutdown */
#define ETOOMANYREFS	(25+TCP_EBASE)	/* Too many references: can't splice */
#define ETIMEDOUT	(26+TCP_EBASE)	/* Connection timed out */
#define ECONNREFUSED	(27+TCP_EBASE)	/* Connection refused */

/* random errors */
#define EHOSTDOWN	(28+TCP_EBASE)	/* Host is down */
#define EHOSTUNREACH	(29+TCP_EBASE)	/* No route to host */
#define	TCP_ELIMIT	EHOSTUNREACH

/*
 * New pathname-handling error numbers.
 */
#define	NAMI_EBASE	TCP_ELIMIT
#define ELOOP		(1+NAMI_EBASE)	/* Too many levels of symbolic links */
#define	ENAMETOOLONG	(2+NAMI_EBASE)	/* File name too long */
#define	NAMI_ELIMIT	ENAMETOOLONG

/*
 * Network filesystem error numbers (nfs).
 */
#define	NFS_EBASE	NAMI_ELIMIT
#define	ENOTEMPTY	(1+NFS_EBASE)	/* Directory not empty */
#define	EDQUOT		(2+NFS_EBASE)	/* Disc quota exceeded */
#define ESTALE		(3+NFS_EBASE)	/* Stale NFS file handle */
#define	ENFSREMOTE	(4+NFS_EBASE)	/* Too many levels of remote in path */
#define	ENFSWFLUSH	(5+NFS_EBASE)	/* NFS write cache flushed */
#define	NFS_ELIMIT	ENFSWFLUSH

#define	LASTERRNO	NFS_ELIMIT

#endif	_SYS_ERRNO_
