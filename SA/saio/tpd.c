#ident "$Header: tpd.c,v 1.2 90/01/17 08:53:30 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * tpd.c -- tape directory routines
 */

#include "sys/param.h"
#include "sys/errno.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

#define ctp(x)	((struct tp_entry *)(x->i_ino_dir))

_tpdinit()
{
	if (sizeof (struct tp_entry) > IOB_INODE)
		_io_abort ("bad size in iob for tpd");
}

/*
 * _tpdopen -- lookup file in boot tape directory
 */
_tpdopen(io, file, flags)
register struct iob *io;
char *file;
int flags;
{
	register struct tp_entry *te;
	struct tp_header th;
	extern char *_get_iobbuf();

	if (flags & F_WRITE) {
		printf("can't write boot tapes, sorry\n");
		goto bad;
	}
	/*
	 * read tape directory
	 */
	io->i_ma = (char *)&th;
	io->i_cc = sizeof(th);
	io->i_bn = 0;
	if ((*io->i_dp->dt_strategy)(io, READ) != sizeof(th)) {
		printf("error on tpdopen\n");
		goto bad;
	}
	if (!is_tpd(&th.th_td)) {
		printf("tape is not in boot tape format\n");
		goto bad;
	}
	/*
	 * Search the tape directory for the requested file
	 */
	ctp(io)->te_nbytes = 0;
	io->i_bn = -1;
	for (te=th.th_td.td_entry; te<&th.th_td.td_entry[TP_NENTRIES]; te++) {
		if (strncmp(file, te->te_name, TP_NAMESIZE) == 0) {
			*ctp(io) = *te;
			/*
			 * Allocate a buffer for block io
			 */
			io->i_buf = _get_iobbuf();
			return(0);
		}
	}

	printf("%s not found, known files are:\n", file);
	for (te=th.th_td.td_entry; te<&th.th_td.td_entry[TP_NENTRIES]; te++) {
		if (te->te_nbytes)
			printf("\t%s\t%d bytes\n", te->te_name, te->te_nbytes);
	}
bad:
	io->i_errno = EIO;
	return(-1);
}

/*
 * _tpdread -- read from a boot tape file
 */
_tpdread(io, buf, cnt)
register struct iob *io;
char *buf;
int cnt;
{
	int bn, cc, ocnt, off, bcnt;

	/*
	 * Make sure we don't read past end of file
	 */
	if (io->i_offset + cnt > ctp(io)->te_nbytes) {
		cnt = ctp(io)->te_nbytes - io->i_offset;
		if (cnt < 0)
			cnt = 0;
	}
	ocnt = cnt;
	while (cnt) {
		/*
		 * get block containing current offset in buffer if necessary
		 */
		bn = (io->i_offset / DEV_BSIZE) + ctp(io)->te_lbn;
		if (io->i_bn != bn) {
			io->i_bn = bn;
			io->i_ma = io->i_buf;
			io->i_cc = DEV_BSIZE;
			cc = (*io->i_dp->dt_strategy)(io, READ);
			if (cc != DEV_BSIZE) {
				printf("error on tpdread\n");
				io->i_errno = EIO;
				return(-1);
			}
		}
		/*
		 * copy appropriate bytes for current buffer
		 * update offset and cnt
		 */
		off = io->i_offset % DEV_BSIZE;
		bcnt = _min(DEV_BSIZE - off, cnt);
		bcopy(&io->i_buf[off], buf, bcnt);
		io->i_offset += bcnt;
		buf += bcnt;
		cnt -= bcnt;
	}
	return(ocnt);
}

/*
 * _tpdclose -- free the buffer
 */
_tpdclose(io)
struct iob *io;
{
	_free_iobbuf(io->i_buf);
}

/*
 * is_tpd -- decide if we're looking at a reasonable volume_header
 */
is_tpd(td)
struct tp_dir *td;
{
	register csum;
	register int *ip;

	if (td->td_magic != TP_MAGIC)
		return(0);

	csum = 0;
	for (ip = (int *)td; ip < (int *)(td + 1); ip++)
		csum += *ip;
	return(csum == 0);
}

