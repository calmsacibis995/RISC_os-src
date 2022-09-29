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
#ident	"$Header: errlst.c,v 1.13.2.5 90/05/10 01:31:16 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/

/*
 * This is the list of error texts. If you change this list,
 * make sure you change the errno symbol list below.
 */

char	*sys_errlist[] = {
	"Error 0",						/* 000 */
	"No permission match",					/* 001 */
	"No such file or directory",				/* 002 */
	"No such process",					/* 003 */
	"Interrupted system service",				/* 004 */
	"I/O error",						/* 005 */
	"No such device or address",				/* 006 */
	"Arg list too long",					/* 007 */
	"Exec format error",					/* 008 */
	"Bad file number",					/* 009 */
	"No child processes",					/* 010 */
	"Resource temporarily unavailable, try again later",	/* 011 */
	"Not enough space",					/* 012 */
	"Permission denied",					/* 013 */
	"Bad address",						/* 014 */
	"Block device required",				/* 015 */
	"Device or resource busy",				/* 016 */
	"File exists",						/* 017 */
	"Cross-device link",					/* 018 */
	"No such device",					/* 019 */
	"Not a directory",					/* 020 */
	"Is a directory",					/* 021 */
	"Invalid argument",					/* 022 */
	"File table overflow",					/* 023 */
	"Too many open files",					/* 024 */
	"Not a character device",				/* 025 */
	"Text file busy",					/* 026 */
	"File too large",					/* 027 */
	"No space left on device",				/* 028 */
	"Illegal seek",						/* 029 */
	"Read-only file system",				/* 030 */
	"Too many links",					/* 031 */
	"Broken pipe",						/* 032 */
	"Argument out of domain",				/* 033 */
	"Result too large",					/* 034 */
	"No message of desired type",				/* 035 */
	"Identifier removed",					/* 036 */
	"Channel number out of range",				/* 037 */
	"Level 2 not synchronized",				/* 038 */
	"Level 3 halted",					/* 039 */
	"Level 3 reset",					/* 040 */
	"Link number out of range",				/* 041 */
	"Protocol driver not attached",				/* 042 */
	"No CSI structure available",				/* 043 */
	"Level 2 halted",					/* 044 */
	"Deadlock situation detected/avoided",			/* 045 */
	"No record locks available",				/* 046 */
	"Error 47",						/* 047 */
	"Error 48",						/* 048 */
	"Error 49",						/* 049 */
	"Bad exchange descriptor",				/* 050 */
	"Bad request descriptor",				/* 051 */
	"Message tables full",					/* 052 */
	"Anode table overflow",					/* 053 */
	"Bad request code",					/* 054 */
	"Invalid slot",						/* 055 */
	"File locking deadlock",				/* 056 */
	"Bad font file format",					/* 057 */
	"Error 58",						/* 058 */
	"Error 59",						/* 059 */
	"Not a stream device",					/* 060 */
	"No data available",					/* 061 */
	"Timer expired",					/* 062 */
	"Out of stream resources",				/* 063 */
	"Machine is not on the network",			/* 064 */
	"Package not installed",				/* 065 */
	"Object is remote",					/* 066 */
	"Link has been severed",				/* 067 */
	"Advertise error",					/* 068 */
	"Srmount error",					/* 069 */
	"Communication error on send",				/* 070 */
	"Protocol error",					/* 071 */
	"Error 72",						/* 072 */
	"Error 73",						/* 073 */
	"Multihop attempted",					/* 074 */
	"Error 75",						/* 075 */
	"Error 76",						/* 076 */
	"Not a data message",					/* 077 */
	"Error 78",						/* 078 */
	"Error 79",						/* 079 */
	"Name not unique on network",				/* 080 */
	"File descriptor in bad state",				/* 081 */
	"Remote address changed",				/* 082 */
	"Can not access a needed shared library",		/* 083 */
	"Accessing a corrupted shared library",			/* 084 */
	".lib section in a.out corrupted",			/* 085 */
	"Attempting to link in more shared libraries than system limit",
								/* 086 */
	"Can not exec a shared library directly",		/* 087 */
	"Too many processes",					/* 088 */
	"Too many users",					/* 089 */
	"Not supported",					/* 090 */
	"Error 91",						/* 091 */
	"Error 92",						/* 092 */
	"Error 93",						/* 093 */
	"Error 94",						/* 094 */
	"Error 95",						/* 095 */
	"Error 96",						/* 096 */
	"Error 97",						/* 097 */
	"Error 98",						/* 098 */
	"Function not implemented",				/* 099 */
	"Error 100",						/* 100 */

/* The following should probably be split off into a separate table, or
 *	at leasted perror() should be made smart enough so that this table
 *	does not have to have holes
 */

/* TCP errors--they start at TCP_EBASE */

		/* non-blocking and interrupt i/o */
	"Operation would block",				/* 101 */
	"Operation now in progress",				/* 102 */
	"Operation already in progress",			/* 103 */

		/* argument errors */
	"Socket operation on non-socket",			/* 104 */
	"Destination address required",				/* 105 */
	"Message too long",					/* 106 */
	"Protocol wrong type for socket",			/* 107 */
	"Option not supported by protocol",			/* 108 */
	"Protocol not supported",				/* 109 */
	"Socket type not supported",				/* 110 */
	"Operation not supported on socket",			/* 111 */
	"Protocol family not supported",			/* 112 */
	"Address family not supported by protocol family",	/* 113 */
	"Address already in use",				/* 114 */
	"Can't assign requested address",			/* 115 */

		/* operational errors */
	"Network is down",					/* 116 */
	"Network is unreachable",				/* 117 */
	"Network dropped connection on reset",			/* 118 */
	"Software caused connection abort",			/* 119 */
	"Connection reset by peer",				/* 120 */
	"No buffer space available",				/* 121 */
	"Socket is already connected",				/* 122 */
	"Socket is not connected",				/* 123 */
	"Can't send after socket shutdown",			/* 124 */
	"Too many references: can't splice",			/* 125 */
	"Connection timed out",					/* 126 */
	"Connection refused",					/* 127 */
	"Host is down",						/* 128 */
	"Host is unreachable",					/* 129 */

/* new namei errors */
	"Too many levels of symbolic links",			/* 130 */
	"File name too long",					/* 131 */

/* NFS errors */
	"Directory not empty",					/* 132 */
	"Disc quota exceeded",					/* 133 */
	"Stale NFS file handle",				/* 134 */
	"Too many levels of remote in path",			/* 135 */
	"NFS write cache flushed",                              /* 136 */
};
int	sys_nerr = { sizeof(sys_errlist)/sizeof(sys_errlist[0]) };

/*
 * The following table contains the symbolic names for each of
 * the errno values. This is used by the new perror formatting
 * stuff.
 */

char	*sys_errnolist[] = {
	"0",
	"EPERM",
	"ENOENT",
	"ESRCH",
	"EINTR",
	"EIO",
	"EXIO",
	"E2BIG",
	"ENOEXEC",
	"EBADF",
	"ECHILD",
	"EAGAIN",
	"ENOMEM",
	"EACCES",
	"EFAULT",
	"ENOTBLK",
	"EBUSY",
	"EEXIST",
	"EXDEV",
	"ENODEV",
	"ENOTDIR",
	"EISDIR",
	"EINVAL",
	"ENFILE",
	"EMFILE",
	"ENOTTY",
	"ETXTBSY",
	"EFBIG",
	"ENOSPC",
	"ESPIPE",
	"EROFS",
	"EMLINK",
	"EPIPE",
	"EDOM",
	"ERANGE",
	"ENOMSG",
	"EIDRM",
	"ECHRNG",
	"EL2NSYNC",
	"E3HLT",
	"E3RST",
	"ELNRNG",
	"EUNATCH",
	"ENOCSI",
	"E2HLT",
	"EDEADLK",
	"ENOLCK",
	"47",
	"48",
	"49",
	"EBADE",
	"EBADR",
	"EXFULL",
	"ENOANO",
	"EBADRQC",
	"EBADSLT",
	"EDEADLOCK",
	"EBFONT",
	"58",
	"59",
	"ENOSTR",
	"ENODATA",
	"ETIME",
	"ENOSR",
	"ENONET",
	"ENOPKG",
	"EREMOTE",
	"ENOLINK",
	"EADV",
	"ESRMNT",
	"ECOMM",
	"EPROTO",
	"72",
	"73",
	"EMULTIHOP",
	"ELBIN",
	"EDOTDOT",
	"EBADMSG",
	"78",
	"79",
	"ENOTUNIQ",
	"EEBADFD",
	"EREMCHG",
	"ELIBACC",
	"ELIBBAD",
	"ELIBSCN",
	"ELIBMAX",
	"ELIBEXEC",
	"EPROCLIM",
	"EUSERS",
	"90",
	"91",
	"92",
	"93",
	"94",
	"95",
	"96",
	"97",
	"98",
	"ENOSYS",
	"100",
	"EWOULDBLOCK",
	"EINPROGRESS",
	"EALREADY",
	"ENOTSOCK",
	"EDESTADDRREQ",
	"EMSGSIZE",
	"EPROTOTYPE",
	"ENOPROTOOPT",
	"EPROTONOSUPPORT",
	"ESOCKTNOSUPPORT",
	"EOPNOTSUPP",
	"EPFNOSUPPORT",
	"EAFNOSUPPORT",
	"EADDRINUSE",
	"EADDRNOTAVAIL",
	"ENETDOWN",
	"ENETUNREACH",
	"ENETRESET",
	"ECONNABORTED",
	"ECONNRESET",
	"ENOBUFS",
	"EISCONN",
	"ENOTCONN",
	"ESHUTDOWN",
	"ETOOMANYREFS",
	"ETIMEDOUT",
	"ECONNREFUSED",
	"EHOSTDOWN",
	"EHOSTUNREACH",
	"ELOOP",
	"ENAMETOOLONG",
	"ENOTEMPTY",
	"EDQUOT",
	"ESTALE",
	"ENFSREMOTE",
	"ENFSWFLUSH",
};
int	sys_nerrno = { sizeof(sys_errnolist)/sizeof(sys_errnolist[0]) };
