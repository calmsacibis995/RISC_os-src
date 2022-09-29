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
#ident	"$Header: pathconf.c,v 1.7.1.5 90/05/10 04:13:43 wje Exp $"

#ifndef lint
#endif

#include <sys/types.h>
#include <sys/sysmips.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>
#include <unistd.h>
#include <errno.h>

extern	int errno;

long
pathconf(path, name)
	unsigned name;
{
	int	fd, ret;
	if ((fd = open(path, O_RDONLY|O_NONBLOCK, 0444)) < 0) {
		return(-1);
	}
	ret = fpathconf(fd, name);
	close(fd);
	return(ret);
}

long
fpathconf(fd, name)
	int 	fd;
	unsigned name;
{
	struct stat statb;

	switch (name) {
	case _PC_LINK_MAX:
		return(sysmips(GET_LINK_MAX, fd));

	case _PC_MAX_CANON:
		if (!isatty(fd)) {
			errno = EINVAL; 
			return(-1);
		}
		return(sysmips(GET_MAX_CANON, fd));

	case _PC_MAX_INPUT:
		if (!isatty(fd)) {
			errno = EINVAL; 
			return(-1);
		}
		return(sysmips(GET_MAX_INPUT, fd));

	case _PC_NAME_MAX:
		return(sysmips(GET_NAME_MAX, fd));

	case _PC_PATH_MAX:
		return(sysmips(GET_PATH_MAX, fd));

	case _PC_PIPE_BUF:
		if (fstat(fd, &statb) < 0) 
			return(-1);
		if (!(S_ISFIFO(statb.st_mode) || S_ISDIR(statb.st_mode))) {
			errno = EINVAL; 
			return(-1);
		}
		return(sysmips(GET_PIPE_BUF, fd));

	case _PC_CHOWN_RESTRICTED:
		return(sysmips(GET_POSIX_CHOWN_RESTRICTED, fd));

	case _PC_NO_TRUNC:
		return(_POSIX_NO_TRUNC);

	case _PC_VDISABLE:
		if (!isatty(fd)) {
			errno = EINVAL; 
			return(-1);
		}
		return(sysmips(GET_POSIX_VDISABLE, fd));

	default:
		errno = EINVAL;
		return(-1);
	}

}
