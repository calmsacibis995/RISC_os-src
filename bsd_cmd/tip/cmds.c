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
#ident	"$Header: cmds.c,v 1.2.2.9 90/05/28 17:52:12 wje Exp $"

#include <a.out.h>
#include "protocol.h"

/*
 * WARNING: this include file must be after a.out.h above because tip.h
 * defines a macro "value(x)" which conflicts with a structure field
 * named value in sym.h (which is included by a.out.h).
 */
#include "tip.h"

#ifndef MIPSEBMAGIC_2
#define MIPSEBMAGIC_2	0x0163
#define MIPSELMAGIC_2	0x0166
#define SMIPSEBMAGIC_2	0x6301
#define SMIPSELMAGIC_2	0x6601
#endif

/*
 * tip
 *
 * miscellaneous commands
 */

int	quant[] = { 60, 60, 24 };

char	null = '\0';
char	*sep[] = { "second", "minute", "hour" };
static char *argv[10];		/* argument vector for take and put */

int	timeout();		/* timeout function called on alarm */
int	stopsnd();		/* SIGINT handler during file transfers */
int	intprompt();		/* used in handling SIG_INT during prompt */
int	intcopy();		/* interrupt routine for file transfers */

/*
 * FTP - remote ==> local
 *  get a file from the remote host
 */
getfl(c)
	char c;
{
	char buf[256], *cp, *expand();
	
	putchar(c);
	/*
	 * get the UNIX receiving file's name
	 */
	if (prompt("Local file name? ", copyname))
		return;
	cp = expand(copyname);
	if ((sfd = creat(cp, 0666)) < 0) {
		printf("\r\n%s: cannot creat\r\n", copyname);
		return;
	}
	
	/*
	 * collect parameters
	 */
	if (prompt("List command for remote system? ", buf)) {
		unlink(copyname);
		return;
	}
	transfer(buf, sfd, value(EOFREAD));
}

/*
 * Cu-like take command
 */
cu_take(cc)
	char cc;
{
	int fd, argc;
	char line[BUFSIZ], *expand(), *cp;

	if (prompt("[take] ", copyname))
		return;
	if ((argc = args(copyname, argv)) < 1 || argc > 2) {
		printf("usage: <take> from [to]\r\n");
		return;
	}
	if (argc == 1)
		argv[1] = argv[0];
	cp = expand(argv[1]);
	if ((fd = creat(cp, 0666)) < 0) {
		printf("\r\n%s: cannot create\r\n", argv[1]);
		return;
	}
	sprintf(line, "cat %s;echo \01", argv[0]);
	transfer(line, fd, "\01");
}

static	jmp_buf intbuf;
/*
 * Bulk transfer routine --
 *  used by getfl(), cu_take(), and pipefile()
 */
transfer(buf, fd, eofchars)
	char *buf, *eofchars;
{
	register int ct;
	char c, buffer[BUFSIZ];
	register char *p = buffer;
	register int cnt, eof;
	time_t start;
	int (*f)();

	pwrite(FD, buf, size(buf));
	quit = 0;
	kill(pid, SIGIOT);
	read(repdes[0], (char *)&ccc, 1);  /* Wait until read process stops */
	
	/*
	 * finish command
	 */
	pwrite(FD, "\r", 1);
	do
		read(FD, &c, 1); 
	while ((c&0177) != '\n');
	ioctl(0, TIOCSETC, &defchars);
	
	(void) setjmp(intbuf);
	f = signal(SIGINT, intcopy);
	start = time(0);
	for (ct = 0; !quit;) {
		eof = read(FD, &c, 1) <= 0;
		c &= 0177;
		if (quit)
			continue;
		if (eof || any(c, eofchars))
			break;
		if (c == 0)
			continue;	/* ignore nulls */
		if (c == '\r')
			continue;
		*p++ = c;

		if (c == '\n' && boolean(value(VERBOSE)))
			printf("\r%d", ++ct);
		if ((cnt = (p-buffer)) == number(value(FRAMESIZE))) {
			if (write(fd, buffer, cnt) != cnt) {
				printf("\r\nwrite error\r\n");
				quit = 1;
			}
			p = buffer;
		}
	}
	if (cnt = (p-buffer))
		if (write(fd, buffer, cnt) != cnt)
			printf("\r\nwrite error\r\n");

	if (boolean(value(VERBOSE)))
		prtime(" lines transferred in ", time(0)-start);
	ioctl(0, TIOCSETC, &tchars);
	write(fildes[1], (char *)&ccc, 1);
	signal(SIGINT, f);
	close(fd);
}

/*
 * FTP - remote ==> local process
 *   send remote input to local process via pipe
 */
pipefile()
{
	int cpid, pdes[2];
	char buf[256];
	int status, p;
	extern int errno;

	if (prompt("Local command? ", buf))
		return;

	if (pipe(pdes)) {
		printf("can't establish pipe\r\n");
		return;
	}

	if ((cpid = fork()) < 0) {
		printf("can't fork!\r\n");
		return;
	} else if (cpid) {
		if (prompt("List command for remote system? ", buf)) {
			close(pdes[0]), close(pdes[1]);
			kill (cpid, SIGKILL);
		} else {
			close(pdes[0]);
			signal(SIGPIPE, intcopy);
			transfer(buf, pdes[1], value(EOFREAD));
			signal(SIGPIPE, SIG_DFL);
			while ((p = wait(&status)) > 0 && p != cpid)
				;
		}
	} else {
		register int f;

		dup2(pdes[0], 0);
		close(pdes[0]);
		for (f = 3; f < 20; f++)
			close(f);
		execute(buf);
		printf("can't execl!\r\n");
		exit(0);
	}
}

/*
 * Interrupt service routine for FTP
 */
stopsnd()
{

	stop = 1;
	signal(SIGINT, SIG_IGN);
}

/*
 * FTP - local ==> remote
 *  send local file to remote host
 *  terminate transmission with pseudo EOF sequence
 */
sendfile(cc)
	char cc;
{
	FILE *fd;
	char *fnamex;
	char *expand();

	putchar(cc);
	/*
	 * get file name
	 */
	if (prompt("Local file name? ", fname))
		return;

	/*
	 * look up file
	 */
	fnamex = expand(fname);
	if ((fd = fopen(fnamex, "r")) == NULL) {
		printf("%s: cannot open\r\n", fname);
		return;
	}
	transmit(fd, value(EOFWRITE), NULL);
	if (!boolean(value(ECHOCHECK))) {
		struct sgttyb buf;

		ioctl(FD, TIOCGETP, &buf);	/* this does a */
		ioctl(FD, TIOCSETP, &buf);	/*   wflushtty */
	}
}

/*
 * Bulk transfer routine to remote host --
 *   used by sendfile() and cu_put()
 */
transmit(fd, eofchars, command)
	FILE *fd;
	char *eofchars, *command;
{
	char *pc, lastc;
	int c, ccount, lcount;
	time_t start_t, stop_t;
	int (*f)();

	kill(pid, SIGIOT);	/* put TIPOUT into a wait state */
	stop = 0;
	f = signal(SIGINT, stopsnd);
	ioctl(0, TIOCSETC, &defchars);
	read(repdes[0], (char *)&ccc, 1);
	if (command != NULL) {
		for (pc = command; *pc; pc++)
			send(*pc);
		if (boolean(value(ECHOCHECK)))
			read(FD, (char *)&c, 1);	/* trailing \n */
		else {
			struct sgttyb buf;

			ioctl(FD, TIOCGETP, &buf);	/* this does a */
			ioctl(FD, TIOCSETP, &buf);	/*   wflushtty */
			sleep(5); /* wait for remote stty to take effect */
		}
	}
	lcount = 0;
	lastc = '\0';
	start_t = time(0);
	while (1) {
		ccount = 0;
		do {
			c = getc(fd);
			if (stop)
				goto out;
			if (c == EOF)
				goto out;
			if (c == 0177 && !boolean(value(RAWFTP)))
				continue;
			lastc = c;
			if (c < 040) {
				if (c == '\n') {
					if (!boolean(value(RAWFTP)))
						c = '\r';
				}
				else if (c == '\t') {
					if (!boolean(value(RAWFTP))) {
						if (boolean(value(TABEXPAND))) {
							send(' ');
							while ((++ccount % 8) != 0)
								send(' ');
							continue;
						}
					}
				} else
					if (!boolean(value(RAWFTP)))
						continue;
			}
			send(c);
		} while (c != '\r' && !boolean(value(RAWFTP)));
		if (boolean(value(VERBOSE)))
			printf("\r%d", ++lcount);
		if (boolean(value(ECHOCHECK))) {
			timedout = 0;
			alarm(value(ETIMEOUT));
			do {	/* wait for prompt */
				read(FD, (char *)&c, 1);
				if (timedout || stop) {
					if (timedout)
						printf("\r\ntimed out at eol\r\n");
					alarm(0);
					goto out;
				}
			} while ((c&0177) != character(value(PROMPT)));
			alarm(0);
		}
	}
out:
	if (lastc != '\n' && !boolean(value(RAWFTP)))
		send('\r');
	for (pc = eofchars; pc && *pc; pc++)
		send(*pc);
	stop_t = time(0);
	fclose(fd);
	signal(SIGINT, f);
	if (boolean(value(VERBOSE)))
		if (boolean(value(RAWFTP)))
			prtime(" chars transferred in ", stop_t-start_t);
		else
			prtime(" lines transferred in ", stop_t-start_t);
	write(fildes[1], (char *)&ccc, 1);
	ioctl(0, TIOCSETC, &tchars);
}

/*
 * Cu-like put command
 */
cu_put(cc)
	char cc;
{
	FILE *fd;
	char line[BUFSIZ];
	int argc;
	char *expand();
	char *copynamex;

	if (prompt("[put] ", copyname))
		return;
	if ((argc = args(copyname, argv)) < 1 || argc > 2) {
		printf("usage: <put> from [to]\r\n");
		return;
	}
	if (argc == 1)
		argv[1] = argv[0];
	copynamex = expand(argv[0]);
	if ((fd = fopen(copynamex, "r")) == NULL) {
		printf("%s: cannot open\r\n", copynamex);
		return;
	}
	if (boolean(value(ECHOCHECK)))
		sprintf(line, "cat>%s\r", argv[1]);
	else
		sprintf(line, "stty -echo;cat>%s;stty echo\r", argv[1]);
	transmit(fd, "\04", line);
}

/*
 * FTP - send single character
 *  wait for echo & handle timeout
 */
send(c)
	char c;
{
	char cc;
	int retry = 0;

	cc = c;
	pwrite(FD, &cc, 1);
#ifdef notdef
	if (number(value(CDELAY)) > 0 && c != '\r')
		nap(number(value(CDELAY)));
#endif
	if (!boolean(value(ECHOCHECK))) {
#ifdef notdef
		if (number(value(LDELAY)) > 0 && c == '\r')
			nap(number(value(LDELAY)));
#endif
		return;
	}
tryagain:
	timedout = 0;
	alarm(value(ETIMEOUT));
	read(FD, &cc, 1);
	alarm(0);
	if (timedout) {
		printf("\r\ntimeout error (%s)\r\n", ctrl(c));
		if (retry++ > 3)
			return;
		pwrite(FD, &null, 1); /* poke it */
		goto tryagain;
	}
}

#ifdef MIPS
load(cc)
char cc;
{
	FILE *fd;
	char *fnamex;
	char *expand();

	/*
	 * get file name
	 */
	if (prompt("[load] Object file name? ", fname))
		return;

	/*
	 * look up file
	 */
	fnamex = expand(fname);
	if ((fd = fopen(fnamex, "r")) == NULL) {
		printf("%s: cannot open\r\n", fname);
		return;
	}
	obj_xmit(fd);
	fclose(fd);
}

static int
hextob(p, r)
 char *p;
 unsigned *r;
{
	unsigned sum = 0;

	if (*p == '0' && (*(p+1) == 'x' || *(p+1) == 'X'))
		p += 2;
	while (*p != '\0') {
		sum <<= 4;
		if (*p >= '0' && *p <='9')
			sum += *p-'0';
		else if (*p >= 'a' && *p <='f')
			sum += *p-'a'+10;
		else if (*p >= 'A' && *p <='F')
			sum += *p-'A'+10;
		else
			return(0);
		p++;
	}
	*r = sum;
	return(1);
}

loaddata(cc)
char cc;
{
	FILE *fd;
	char *fnamex;
	char *expand();
	char hexstring[80];
	unsigned taddr;

	/*
	 * get file name
	 */
	if (prompt("\n\r[loaddata] data file name? ", fname))
		return;

	/*
	 * look up file
	 */
	fnamex = expand(fname);
	if ((fd = fopen(fnamex, "r")) == NULL) {
		printf("%s: cannot open\r\n", fname);
		return;
	}
	/*
	 * get target load address
	 */
	if (prompt("[loaddata] target address (hex)? ", hexstring))
		goto out;
	
	if (!hextob(hexstring, &taddr)) {
		printf("badly formed address\r\n", fname);
		goto out;
	}
	printf("Downloading %s to 0x%x\r\n", fnamex, taddr);
	data_xmit(fd, taddr);
out:
	fclose(fd);
}

static struct load_pkt {
	char	lp_cmd;
	char	lp_buf[MAXPACKET];
} load_pkt;

static char proto_buf[MAXPACKET];
static jmp_buf xmit_buf;

/*
 * format of bootable a.out file headers
 */
struct execinfo {
	struct filehdr fh;
	AOUTHDR ah;
};

static
obj_xmit(fd)
FILE *fd;
{
	time_t start_t, stop_t;
	struct execinfo ei;
	int hostsex;
	int pktsize;
	int stopxmit();

	kill(pid, SIGIOT);	/* put TIPOUT into a wait state */
	if (setjmp(xmit_buf)) {
		printf("INTERRUPT\n");
		goto out;
	}
	signal(SIGINT, stopxmit);
	ioctl(0, TIOCSETC, &defchars);
	read(repdes[0], (char *)&ccc, 1);

	start_t = time(0);

	if (fread(&ei, sizeof(ei), 1, fd) != 1) {
		printf("File too short to be a.out format\n");
		goto out;
	}

	hostsex = gethostsex();

	switch (ei.fh.f_magic) {
	case MIPSELMAGIC:
	case MIPSEBMAGIC:
	case MIPSELMAGIC_2:
	case MIPSEBMAGIC_2:
		break;

	case SMIPSELMAGIC:
	case SMIPSEBMAGIC:
	case SMIPSELMAGIC_2:
	case SMIPSEBMAGIC_2:
		swap_aouthdr(&ei.ah, hostsex);
		break;

	default:
		printf("Bad file header magic number: 0x%x\n", ei.fh.f_magic);
		goto out;
	}

	if (N_BADMAG(ei.ah)) {
		printf("Bad aouthdr magic number 0x%x\n", ei.ah.magic);
		goto out;
	}

	init_proto(FD);

	/*
	 * Set initial PC to entry point
	 */
	load_pkt.lp_cmd = 'P';
	pktsize = 1 + btoh(ei.ah.entry, load_pkt.lp_buf);
	if (putpkt(FD, &load_pkt, pktsize, DATA_PKTTYPE) < 0)
		goto rmt_fail;

	/*
	 * Send text segment
	 */
	printf("Sending text segment\r\n");
	load_pkt.lp_cmd = 'B';
	pktsize = 1 + btoh(ei.ah.text_start, load_pkt.lp_buf);
	if (putpkt(FD, &load_pkt, pktsize, DATA_PKTTYPE) < 0)
		goto rmt_fail;

	fseek(fd, N_TXTOFF(ei.fh, ei.ah), 0);
	if (send_seg(ei.ah.tsize, fd) < 0)
		goto out;

	/*
	 * Send data segment
	 */
	printf("\r\nSending data segment\r\n");
	load_pkt.lp_cmd = 'B';
	pktsize = 1 + btoh(ei.ah.data_start, load_pkt.lp_buf);
	if (putpkt(FD, &load_pkt, pktsize, DATA_PKTTYPE) < 0)
		goto rmt_fail;

	if (send_seg(ei.ah.dsize, fd) < 0)
		goto out;
	
	/*
	 * Send commands to zero the bss segment
	 */
	load_pkt.lp_cmd = 'B';
	pktsize = 1 + btoh(ei.ah.bss_start, load_pkt.lp_buf);
	if (putpkt(FD, &load_pkt, pktsize, DATA_PKTTYPE) < 0)
		goto rmt_fail;

	load_pkt.lp_cmd = 'Z';
	pktsize = 1 + btoh(ei.ah.bsize, load_pkt.lp_buf);
	if (putpkt(FD, &load_pkt, pktsize, DATA_PKTTYPE) < 0)
		goto rmt_fail;

	if (boolean(value(VERBOSE))) {
		stop_t = time(0);
		printf("\r\n%d", ei.ah.tsize + ei.ah.dsize);
		prtime(" bytes transferred in ", stop_t-start_t);
	}
out:
	/*
	 * Terminate protocol connection
	 * by sending 'E' packet
	 */
	load_pkt.lp_cmd = 'E';
	if (putpkt(FD, &load_pkt, 1, DATA_PKTTYPE) < 0) {
rmt_fail:
		printf("\r\nremote does not respond\r\n");
	}

	signal(SIGINT, SIG_DFL);
	write(fildes[1], (char *)&ccc, 1);
	ioctl(0, TIOCSETC, &tchars);
}

static
data_xmit(fd, taddr)
FILE *fd;
unsigned taddr;
{
	time_t start_t, stop_t;
	int pktsize;
	int stopxmit();
	struct stat stat;

	if (fstat(fileno(fd), &stat) == -1) {
		printf("cannot stat data file\r\n");
		goto out;
	}
	kill(pid, SIGIOT);	/* put TIPOUT into a wait state */
	if (setjmp(xmit_buf)) {
		printf("INTERRUPT\n");
		goto out;
	}
	signal(SIGINT, stopxmit);
	ioctl(0, TIOCSETC, &defchars);
	read(repdes[0], (char *)&ccc, 1);

	start_t = time(0);

	init_proto(FD);

	/*
	 * Send raw data
	 */
	printf("Sending data\r\n");
	load_pkt.lp_cmd = 'B';
	pktsize = 1 + btoh(taddr, load_pkt.lp_buf);
	if (putpkt(FD, &load_pkt, pktsize, DATA_PKTTYPE) < 0)
		goto rmt_fail;

	if (send_seg(stat.st_size, fd) < 0)
		goto out;


	if (boolean(value(VERBOSE))) {
		stop_t = time(0);
		printf("\r\n%d", stat.st_size);
		prtime(" bytes transferred in ", stop_t-start_t);
	}
out:
	/*
	 * Terminate protocol connection
	 * by sending 'E' packet
	 */
	load_pkt.lp_cmd = 'E';
	if (putpkt(FD, &load_pkt, 1, DATA_PKTTYPE) < 0) {
rmt_fail:
		printf("\r\nremote does not respond\r\n");
	}

	signal(SIGINT, SIG_DFL);
	write(fildes[1], (char *)&ccc, 1);
	ioctl(0, TIOCSETC, &tchars);
}

send_seg(remaining_bytes, fd)
int remaining_bytes;
FILE *fd;
{
	int pktsize;
	int d_bytes;
	int desired_bytes;
	int tries;

	tries = 0;
	if (boolean(value(VERBOSE)))
		printf("\r%d   ", remaining_bytes);
	while (remaining_bytes > 0) {
		/*
		 * proto_pktsize() returns "optimum" packet size given
		 * current line conditions.
		 * If not using 8 bit path, divide d_bytes by 2 since it
		 * takes 2 characters to represent one byte of data
		 * when using 7 bit path
		 */
		desired_bytes = proto_pktsize();
		if (!boolean(value(BITS8)))
			desired_bytes /= 2;
		d_bytes = min(desired_bytes-2, remaining_bytes);
			
		if (boolean(value(BITS8))) {	/* binary data packet */
			load_pkt.lp_cmd = 'd';
			if (fread(load_pkt.lp_buf, 1, d_bytes, fd) != d_bytes) {
				printf("Short read from object file\n");
				return(-1);
			}
			pktsize = 1 + d_bytes;
		} else {			/* hex data packet */
			if (fread(proto_buf, 1, d_bytes, fd) != d_bytes) {
				printf("Short read from object file\n");
				return(-1);
			}
			load_pkt.lp_cmd = 'D';
			pktsize = 1 + blktoh(proto_buf,load_pkt.lp_buf,d_bytes);
		}
		if (putpkt(FD, &load_pkt, pktsize, DATA_PKTTYPE) < 0) {
			if (fseek(fd, -d_bytes, 1) < 0) {
				printf("Can't seek in a.out file\n");
				return(-1);
			}
			if (++tries > 9) {
				printf("\n\rremote does not respond\n\r");
				return(-1);
			}
			continue;
		}
		tries = 0;
		remaining_bytes -= d_bytes;
		if (boolean(value(VERBOSE)))
			printf("\r%d   ", remaining_bytes);
	}
	return(0);
}

static char hexdigit[] = "0123456789abcdef";

/*
 * btoh -- convert binary integer to ascii hex representation
 * Returns number of ascii characters in representation
 */
static
btoh(val, cp)
register unsigned val;
register char *cp;
{
	char buf[8];
	register char *bp;
	register int i;

	bp = buf;
	while (val) {
		*bp++ = hexdigit[val & 0xf];
		val >>= 4;
	}
	i = 0;
	while (--bp >= buf) {
		*cp++ = *bp;
		i++;
	}
	return(i);
}

/*
 * blktoh -- convert block of binary data to ascii hex representation
 * Returns number of bytes in ascii hex representation
 */
static
blktoh(srcp, dstp, s_count)
register char *srcp;
register char *dstp;
int s_count;
{
	register i = s_count;
	register c;

	while (i-- > 0) {
		c = *srcp++;
		*dstp++ = hexdigit[(c >> 4) & 0xf];
		*dstp++ = hexdigit[c & 0xf];
	}
	return(s_count * 2);
}

/*
 * set_timer -- arrange for timer_intr() to be called in future
 * (needed for protocol support)
 */
set_timer(secs)
{
	int timer_intr();

	signal(SIGALRM, timer_intr);
	alarm(secs);
}

int *timer_jmpbuf;

/*
 * timer_intr -- called on timer expiration
 * (needed for protocol support)
 */
timer_intr()
{
	if (timer_jmpbuf)
		longjmp(timer_jmpbuf, 1);
	printf("TIMER ERROR\n");
}

/*
 * min -- return min value of two args
 */
min(i, j)
{
	return (i < j ? i : j);
}

/*
 * max -- return max value of two args
 */
max(i, j)
{
	return (i > j ? i : j);
}

/*
 * Input buffering for protocol i/o
 */
static unsigned char pgetbuf[BUFSIZ];
static int pgetcnt;
static unsigned char *pgetbufp;

/*
 * pgetc -- protocol getc, handles buffering necessary
 */
pgetc(fd)
int fd;
{
	if (pgetcnt <= 0) {
		pgetbufp = pgetbuf;
		pgetcnt = read(fd, pgetbuf, sizeof(pgetbuf));
	}
	pgetcnt--;
	return(*pgetbufp++);
}

/*
 * output buffering for protocol code
 */
static unsigned char pputbuf[MAXPACKET+MAXPKTOVERHEAD];
static int pputcnt;
static unsigned char *pputbufp;

/*
 * pputc -- protocol putc, insert character into protocol output buffer
 */
pputc(c, fd)
char c;
int fd;
{
	*pputbufp++ = c;
	pputcnt++;
	if (pputcnt >= sizeof(pputbuf)) {
		write(fd, pputbuf, pputcnt);
		pputcnt = 0;
		pputbufp = pputbuf;
	}
}

/*
 * putflush -- flush protocol output buffer
 */
putflush(fd)
int fd;
{
	if (pputcnt)
		write(fd, pputbuf, pputcnt);
	pputcnt = 0;
	pputbufp = pputbuf;
}

/*
 * pinit -- initialize protocol buffers
 */
pinit(fd)
int fd;
{
	pgetcnt = pputcnt = 0;
	pputbufp = pputbuf;
}

stopxmit()
{
	/*
	 * There's a race here with the alarm signal.  Really
	 * should declare all this stuff with sigvec(), but then
	 * it wouldn't work on SysV.
	 */
	timer_jmpbuf = 0;
	alarm(0);
	longjmp(xmit_buf, 1);
}
#endif

timeout()
{
	signal(SIGALRM, timeout);
	timedout = 1;
}

/*
 * Stolen from consh() -- puts a remote file on the output of a local command.
 *	Identical to consh() except for where stdout goes.
 */
pipeout(c)
{
	char buf[256];
	int cpid, status, p;
	time_t start;

	putchar(c);
	if (prompt("Local command? ", buf))
		return;
	kill(pid, SIGIOT);	/* put TIPOUT into a wait state */
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	ioctl(0, TIOCSETC, &defchars);
	read(repdes[0], (char *)&ccc, 1);
	/*
	 * Set up file descriptors in the child and
	 *  let it go...
	 */
	if ((cpid = fork()) < 0)
		printf("can't fork!\r\n");
	else if (cpid) {
		start = time(0);
		while ((p = wait(&status)) > 0 && p != cpid)
			;
	} else {
		register int i;

		dup2(FD, 1);
		for (i = 3; i < 20; i++)
			close(i);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		execute(buf);
		printf("can't find `%s'\r\n", buf);
		exit(0);
	}
	if (boolean(value(VERBOSE)))
		prtime("away for ", time(0)-start);
	write(fildes[1], (char *)&ccc, 1);
	ioctl(0, TIOCSETC, &tchars);
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
}


#ifdef CONNECT
/*
 * Fork a program with:
 *  0 <-> local tty in
 *  1 <-> local tty out
 *  2 <-> local tty out
 *  3 <-> remote tty in
 *  4 <-> remote tty out
 */
consh(c)
{
	char buf[256];
	int cpid, status, p;
	time_t start;

	putchar(c);
	if (prompt("Local command? ", buf))
		return;
	kill(pid, SIGIOT);	/* put TIPOUT into a wait state */
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	ioctl(0, TIOCSETC, &defchars);
	read(repdes[0], (char *)&ccc, 1);
	/*
	 * Set up file descriptors in the child and
	 *  let it go...
	 */
	if ((cpid = fork()) < 0)
		printf("can't fork!\r\n");
	else if (cpid) {
		start = time(0);
		while ((p = wait(&status)) > 0 && p != cpid)
			;
	} else {
		register int i;

		dup2(FD, 3);
		dup2(3, 4);
		for (i = 5; i < 20; i++)
			close(i);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		execute(buf);
		printf("can't find `%s'\r\n", buf);
		exit(0);
	}
	if (boolean(value(VERBOSE)))
		prtime("away for ", time(0)-start);
	write(fildes[1], (char *)&ccc, 1);
	ioctl(0, TIOCSETC, &tchars);
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
}
#endif

/*
 * Escape to local shell
 */
shell()
{
	int shpid, status;
	extern char **environ;
	char *cp;

	printf("[sh]\r\n");
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	unraw();
	if (shpid = fork()) {
		while (shpid != wait(&status));
		raw();
		printf("\r\n!\r\n");
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		return;
	} else {
		signal(SIGQUIT, SIG_DFL);
		signal(SIGINT, SIG_DFL);
		if ((cp = rindex(value(SHELL), '/')) == NULL)
			cp = value(SHELL);
		else
			cp++;
		setuid(geteuid());
		setgid(getegid());
		execl(value(SHELL), cp, 0);
		printf("\r\ncan't execl!\r\n");
		exit(1);
	}
}

/*
 * TIPIN portion of scripting
 *   initiate the conversation with TIPOUT
 */
setscript()
{
	char c;
	/*
	 * enable TIPOUT side for dialogue
	 */
	kill(pid, SIGEMT);
	if (boolean(value(SCRIPT)))
	{
#ifdef MIPS
		/*
		 * MIPS mod: first two characters of script message are:
		 *
		 *	'a' or 't' for append or truncate script file
		 *	'd' or 's' for display or silent scripting
		 */
		write(fildes[1], boolean(value(APPEND)) ? "a" : "t", 1);
		write(fildes[1], boolean(value(DISPLAY)) ? "d" : "s", 1);
#endif
		write(fildes[1], value(RECORD), size(value(RECORD)));
	}
	write(fildes[1], "\n", 1);
	/*
	 * wait for TIPOUT to finish
	 */
	read(repdes[0], &c, 1);
	if (c == 'n')
		printf("can't create %s\r\n", value(RECORD));
}

/*
 * Change current working directory of
 *   local portion of tip
 */
chdirectory()
{
	char dirname[80];
	register char *cp = dirname;

	if (prompt("[cd] ", dirname)) {
		if (stoprompt)
			return;
		cp = value(HOME);
	}
	if (chdir(cp) < 0)
		printf("%s: bad directory\r\n", cp);
	printf("!\r\n");
}

abort(msg)
	char *msg;
{
	kill(pid, SIGTERM);
	setreuid(euid, euid);
	setregid(egid, egid);
	disconnect(msg);
	if (msg != NOSTR)
		printf("\r\n%s", msg);
	printf("\r\n[EOT]\r\n");
	delock(uucplock);
	unraw();
	exit(0);
}

finish()
{
	char *dismsg;

	if ((dismsg = value(DISCONNECT)) != NOSTR) {
		write(FD, dismsg, strlen(dismsg));
		sleep(5);
	}
	abort(NOSTR);
}

intcopy()
{

	raw();
	quit = 1;
	longjmp(intbuf, 1);
}

execute(s)
	char *s;
{
	register char *cp;

	if ((cp = rindex(value(SHELL), '/')) == NULL)
		cp = value(SHELL);
	else
		cp++;
	execl(value(SHELL), cp, "-c", s, 0);
}

args(buf, a)
	char *buf, *a[];
{
	register char *p = buf, *start;
	register char **parg = a;
	register int n = 0;

	do {
		while (*p && (*p == ' ' || *p == '\t'))
			p++;
		start = p;
		if (*p)
			*parg = p;
		while (*p && (*p != ' ' && *p != '\t'))
			p++;
		if (p != start)
			parg++, n++;
		if (*p)
			*p++ = '\0';
	} while (*p);

	return(n);
}

prtime(s, a)
	char *s;
	time_t a;
{
	register i;
	int nums[3];

	for (i = 0; i < 3; i++) {
		nums[i] = (int)(a % quant[i]);
		a /= quant[i];
	}
	printf("%s", s);
	while (--i >= 0)
		if (nums[i] || i == 0 && nums[1] == 0 && nums[2] == 0)
			printf("%d %s%c ", nums[i], sep[i],
				nums[i] == 1 ? '\0' : 's');
	printf("\r\n!\r\n");
}

variable()
{
	char	buf[256];

	if (prompt("[set] ", buf))
		return;
	vlex(buf);
	if (vtable[BEAUTIFY].v_access&CHANGED) {
		vtable[BEAUTIFY].v_access &= ~CHANGED;
		kill(pid, SIGSYS);
	}
	if (vtable[SCRIPT].v_access&CHANGED) {
		vtable[SCRIPT].v_access &= ~CHANGED;
		setscript();
		/*
		 * So that "set record=blah script" doesn't
		 *  cause two transactions to occur.
		 */
		if (vtable[RECORD].v_access&CHANGED)
			vtable[RECORD].v_access &= ~CHANGED;
	}
	if (vtable[RECORD].v_access&CHANGED) {
		vtable[RECORD].v_access &= ~CHANGED;
		if (boolean(value(SCRIPT)))
			setscript();
	}
	if (vtable[TAND].v_access&CHANGED) {
		vtable[TAND].v_access &= ~CHANGED;
		if (boolean(value(TAND)))
			tandem("on");
		else
			tandem("off");
	}
 	if (vtable[LECHO].v_access&CHANGED) {
 		vtable[LECHO].v_access &= ~CHANGED;
 		HD = boolean(value(LECHO));
 	}
	if (vtable[PARITY].v_access&CHANGED) {
		vtable[PARITY].v_access &= ~CHANGED;
		setparity();
	}
#ifdef RISCOS
	if (vtable[XONXOFF].v_access&CHANGED) {
		vtable[XONXOFF].v_access &= ~CHANGED;
		if (boolean(value(XONXOFF)))
			change_host_ixon(1);
		else
			change_host_ixon(0);
	}
	if (vtable[LOCAL].v_access&CHANGED &&
	    defclocal != -1) {
		vtable[LOCAL].v_access &= ~CHANGED;
		if (boolean(value(LOCAL)))
			change_host_clocal(1);
		else
			change_host_clocal(0);
	}
#endif RISCOS
}

/*
 * Turn tandem mode on or off for remote tty.
 */
tandem(option)
	char *option;
{
	struct sgttyb rmtty;

	ioctl(FD, TIOCGETP, &rmtty);
	if (strcmp(option,"on") == 0) {
		rmtty.sg_flags |= TANDEM;
		arg.sg_flags |= TANDEM;
	} else {
		rmtty.sg_flags &= ~TANDEM;
		arg.sg_flags &= ~TANDEM;
	}
	ioctl(FD, TIOCSETP, &rmtty);
	ioctl(0,  TIOCSETP, &arg);
}

/*
 * Send a break.
 */
genbrk()
{

	ioctl(FD, TIOCSBRK, NULL);
	sleep(1);
	ioctl(FD, TIOCCBRK, NULL);
}

/*
 * Suspend tip
 */
suspend(c)
	char c;
{

	unraw();
	kill(c == CTRL('y') ? getpid() : 0, SIGTSTP);
	raw();
}

/*
 *	expand a file name if it includes shell meta characters
 */

char *
expand(name)
	char name[];
{
	static char xname[BUFSIZ];
	char cmdbuf[BUFSIZ];
	register int pid, l, rc;
	register char *cp, *Shell;
	int s, pivec[2], (*sigint)();

	if (!anyof(name, "~{[*?$`'\"\\"))
		return(name);
	/* sigint = signal(SIGINT, SIG_IGN); */
	if (pipe(pivec) < 0) {
		perror("pipe");
		/* signal(SIGINT, sigint) */
		return(name);
	}
	sprintf(cmdbuf, "echo %s", name);
	if ((pid = vfork()) == 0) {
		Shell = value(SHELL);
		if (Shell == NOSTR)
			Shell = "/bin/sh";
		close(pivec[0]);
		close(1);
		dup(pivec[1]);
		close(pivec[1]);
		close(2);
		execl(Shell, Shell, "-c", cmdbuf, 0);
		_exit(1);
	}
	if (pid == -1) {
		perror("fork");
		close(pivec[0]);
		close(pivec[1]);
		return(NOSTR);
	}
	close(pivec[1]);
	l = read(pivec[0], xname, BUFSIZ);
	close(pivec[0]);
	while (wait(&s) != pid);
		;
	s &= 0377;
	if (s != 0 && s != SIGPIPE) {
		fprintf(stderr, "\"Echo\" failed\n");
		return(NOSTR);
	}
	if (l < 0) {
		perror("read");
		return(NOSTR);
	}
	if (l == 0) {
		fprintf(stderr, "\"%s\": No match\n", name);
		return(NOSTR);
	}
	if (l == BUFSIZ) {
		fprintf(stderr, "Buffer overflow expanding \"%s\"\n", name);
		return(NOSTR);
	}
	xname[l] = 0;
	for (cp = &xname[l-1]; *cp == '\n' && cp > xname; cp--)
		;
	*++cp = '\0';
	return(xname);
}

/*
 * Are any of the characters in the two strings the same?
 */

anyof(s1, s2)
	register char *s1, *s2;
{
	register int c;

	while (c = *s1++)
		if (any(c, s2))
			return(1);
	return(0);
}
