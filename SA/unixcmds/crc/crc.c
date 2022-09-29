/*	%Q%	%I%	%M%
 *
 * Copyright 1985 by MIPS Computer Systems, Inc.
 *
 * crc.c - computer remote control for data-io prom blower
 */

#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#if !defined(BSD) && !defined(SYSV)
#define SYSV
#endif
#ifdef SYSV
#include <termio.h>
#include <sys/ioctl.h>
#endif SYSV
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#ifdef BSD
#include <sgtty.h>
#endif BSD


char 		*prompt="- ";		/* user prompt */
int		promfd;			/* file descriptor to talk to prom */
int		bytes_in_file;		/* number of data bytes in last -I */
jmp_buf		jumpbuf;		/* upon interrupt longjmp buf */
char		*ttyfile;
char		*error_file = 		/* error info file */
		    "/usr/lib/dataio.errors";

extern int	errno;			/* system error number */
void 		handler();		/* interrupt handler */
int		reopen;
#ifdef SYSV
#include <memory.h>
int didsave = 0;
struct termio saveterm;		/* Save away current setting for reset */
#endif

main(argc, argv)
long	argc;
char	*argv[];
{
    char	input[1024];		/* used to get input lines */

    if (argc < 2) {
	fprintf(stderr, "Usage: crc tty [error_file]\n");
	exit(1);
    } /* if */

    /* get substitue error file name if present */
    if (argc == 3)
	error_file = argv[2];

    /* open fd to prom and set up at 4800 baud */
    ttyfile = argv[1];
    promfd = open(ttyfile, O_RDWR|O_NDELAY);
    if (promfd < 0)
	error("cannot open file %s (%d)\n", ttyfile, errno);
    ttysetup(promfd, B4800);

    /* interrupt signal handler init */
    setjmp(jumpbuf);
    signal(SIGINT,handler);

    /* initial prompt */
    fprintf(stdout, "\n%s", prompt); fflush(stdout);

    /* control loop */
    while(fgets(input, sizeof(input), stdin)) {
	process_command(input, 0);
	fprintf(stdout, prompt); fflush(stdout);
    } /* while */

    exit(0);
} /* main */

/*
 * Set up the "remote" tty's state
 */
ttysetup(fncomm, speed)
	int speed;
{
#ifdef BSD
	unsigned	bits = LDECCTQ;
	struct sgttyb	arg;
	struct tchars	tchars;
	int		flags;

	arg.sg_ispeed = arg.sg_ospeed = speed;
	/* definitely do not want parity! */
	arg.sg_flags = CBREAK|DECCTQ|LITOUT|ANYP;
	ioctl(fncomm, TIOCSETP, (char *)&arg);
	ioctl(fncomm, TIOCGETC, (char *)&tchars);
	tchars.t_startc = 0x11;
	tchars.t_stopc = 0x13;
	ioctl(fncomm, TIOCSETC, (char *)&tchars);
	flags = fcntl(fncomm, F_GETFL);
	fcntl(fncomm, F_SETFL, flags|FNDELAY);
#endif BSD

#ifdef SYSV
	struct termio term;

	ioctl(fncomm, TCGETA, &term);
	/* 
	 * Save away for when we're done, but only the first time. 
	 * Reopentty calls this too.
	 */
	if( !didsave ){
	    memcpy(&saveterm, &term, sizeof(struct termio));
	    didsave = 1;
	}
	
	term.c_iflag = IXON | IXOFF;
	term.c_oflag = 0;
	term.c_cflag = speed | CS8 | CREAD | HUPCL | CLOCAL;
	term.c_lflag = 0;
	term.c_cc[VMIN] = 1;
	term.c_cc[VTIME] = 0;

	ioctl(fncomm, TCSETA, &term);
#endif SYSV
}

get_lines(stfd)
FILE	*stfd;
{
	char	line[1024];
	int	lines = 0;
	int	where;

	/* return the number of lines in the file opened in the stdfile
	 *	descriptor argument.
	 */

	/* seek to beginning of file */
	where = ftell(stfd);
	fseek(stfd, 0, 0);

	/* read the lines, counting */
	while(fgets(line, sizeof(line), stfd))
	    lines++;
	
	/* clear EOF, seek to original location, and return result */
	clearerr(stfd);
	fseek(stfd, where, 0);
	return lines;
} /* get_lines */


char *
error_lookup(string)
char	*string;			/* error string from data-io */
{
    int		length;			/* length of error string */
    FILE	*stdefile;		/* stdfd to access error file */
    static char	line[1024];		/* read lines from error file */

    /* When you get an error on the data-io prom blower through crc, it
     *	just return an "F". So the guy who calls me will do an "X"
     *	command upon encountering a "F" result. The X command returns
     *	output of the form: "...HHHHHH>", where the H's are hex numbers.
     *	The H's come in pairs and refer to errors from the data-io
     *	manuals. The last pair of H's before the '>' is the most recent.
     *	The H pairs build up until there is no more room or a X command
     *	is executed.
     *
     * This routine strips off the last pair of H's (if there) and looks
     *	up the error code in the error_file. We either return the line
     *	in the error file or the input string argument.
     *
     *	The error file has the format of:
     *
     *		HH<tab>anything
     *		...
     *
     *	where the HH starts in the first column.
     */

    length = strlen(string);

    if(length < 4) {
	/* probably just got a '>' */
	return string;
    } else if (string[length-1] != '\n' || string[length-2] != '>') {
	/* need to see trailing ">\n", otherwise botched input */
	return string;
    } /* if */

    /* open error file */
    stdefile = fopen(error_file, "r");
    if (stdefile == NULL)
	return string;

    /* read through file, checking for a matching code */
    while(fgets(line, sizeof(line), stdefile)) {
	if (line[0] == string[length-4] && line[1] == string[length-3]) {
	    fclose(stdefile);
	    return line;
	} /* if */
    } /* while */

    /* error not found, close error file and return input string */
    fclose(stdefile);
    return string;
} /* error_lookup */


process_command(input, result)
char	*input;			/* user input string */
char	*result;		/* non-0 => send data-io output back in this */
{
    char	buf[1024];		/* temporary buffer */
    char	output;			/* read from data-io */
    char	lastchar;		/* last output char */
    int		length;			/* for lengths of strings */
    static int	exit_on_fail = 0;	/* set if exit program on "F" */

    length = strlen(input);

    if (input[0] == '?') {
	help();
	return;		/* no output from data-io */
    } else if (input[0] == '\033') {
	/* don't send \r to data-io for <esc> */
	if (mywrite(promfd, "\033", 1, 0) != 1)
	    error("cannot write line to port (%d)\n", errno);
	/* skip down to reading output */
    } else if (input[0] == '-') {
	/* this is a crc program command, not a data-io command */
	switch(input[1]) {
	case 'q':					/* quit */
	    exit(1);
	case '-':					/* comment */
	    /* comment, just print it */
	    fprintf(stdout, "%s", input+2);
	    return;
	case 'D':					/* block set */
	    /* set the number of blocks to the number of bytes in file
	     *	divided by the argument.
	     */
	    sprintf(buf, "%x;\n", bytes_in_file / atoi(input+2));
	    upper_case(buf);
	    process_command(buf, 0);
	    return;
	case 'E':					/*exit on fail toggle */
	    exit_on_fail = !exit_on_fail;
	    fprintf(stdout, "exit on fail = %s\n", exit_on_fail?
		"true":"false");
	    return;
	case 'I':				/* copy file to ram */
	case 'B':				/* copy jedec file to ram */
	    input[length-1] = '\0';
	    if (file_to_ram(input+2, input+1) == 0) {
		if (exit_on_fail)
		    exit(1);
		else
		    return;
	    } /* if */
	    break;
	default:					/* unknown */
	    fprintf(stderr, "unknown crc command\n");
	    return;
	} /* switch */
    } else {
	/* data-io command */

	/* change <nl> to <cr> for data-io */
	if(input[length-1] == '\n')
	    input[length-1] = '\r';
	/* write the line out, so short should not have to retry */
	if (mywrite(promfd, input, length, 0) != length)
	    error("cannot write line to port (%d)\n", errno);
    } /* if */


    /* now read output from prom blower */

    /* read first character */
    myread(promfd, &output, 1);

    if (result)
	*result++ = output;
    else if (output != 'F')
	fprintf(stdout, "%c", output);

    if (output == 'F') {
	char		Xbuf[1024];	/* output from X command */
	static int	inX;		/* stop infinite loop */

	if (inX) {
	    fprintf(stdout, "Exiting-- loop in X command after failure\n");
	    exit(1);
	} /* if */

	/* read and skip the \r from the 'F' */
	myread(promfd, &output, 1);

	fprintf(stdout, "Note pack lights, then hit <cr> for error report: ");
	fflush(stdout);
	{
	    static FILE		*stdtty = NULL;

	    if (isatty(fileno(stdin))) {
		stdtty = stdin;
	    } else if(stdtty == NULL) {
		stdtty = fopen("/dev/tty", "r");
	    } /* if */

	    if (stdtty == NULL) {
		fprintf(stdout, "error display did not wait for tty input because cannot open /dev/tty\n");
	    } else {
		fgets(Xbuf, sizeof(Xbuf), stdtty);
	    } /* if */
	} /* block */


	/* X command returns error codes */
	inX = 1;
	sleep(1);
	process_command("X\n", Xbuf);
	inX = 0;

	fprintf(stdout, "Failure: %s\n", error_lookup(Xbuf));
	if (exit_on_fail) {
	    fprintf(stdout, "Exiting due to failure\n");
	    exit(1);
	} /* if */

	return;
#ifdef BSD
    } else if (index(">?", output)) {
#endif BSD
#ifdef SYSV
    } else if (strchr(">?", output)) {
#endif SYSV
	output = '>';		/* set so we know to stop */
    } else {
	output = 0;
    } /* if */

    /* process the rest of the output */
    do {
	lastchar = output;

	/* read a char, when its ready */
	myread(promfd, &output, 1);

	/* convert <cr> back to <nl> for printf output stuff */
	if (output == '\r')
		output = '\n';

	if (result)
	    *result++ = output;
	else
	    fprintf(stdout, "%c", output);
    } while (output != '\n' || lastchar != '>');

    /* finish up string or flush buffer and return */
    if (result)
	*result = '\0';
    else
	fflush(stdout);

} /* process_command */


file_to_ram(filename, command)
char	*filename;
char	*command;
{
    FILE	*stdinput;
    char	buf[1024];
    char	line[1024];
    char	*pbuf;
    int		count;
    int		isatty_flag;
    int		length;
    int		base;
    int		result;
    struct stat	statb;
    int		i;
    unsigned short	csum;
#ifdef BSD
    struct sgttyb	arg;
#endif BSD
#ifdef SYSV
	struct termio term;
#endif SYSV
    int		nolines = 0;

    /* input to ram */

    /* open host file */
    stdinput = fopen(filename, "r");
    if (stdinput == NULL) {
	fprintf(stderr, "cannot open file %s (%d)\n", filename, errno);
	return 0;	/* fail */
    } /* if */

    /* set parameters back down to bytes to receive correct amount */
    nolines = get_lines(stdinput);
    bytes_in_file = (nolines - 1) * 16;
    sprintf(buf, "%x;\n", bytes_in_file);
    upper_case(buf);

    /* input command for data-io */
    switch (*command) {
    case 'I':
	fprintf(stdout, "setting RAM base address to 0;\n");
	fprintf(stdout, "setting word size to 08;\n");
	fprintf(stdout, "setting set size to 1;\n");
	fprintf(stdout, "setting number of words to 0x%s", buf);
	fprintf(stdout, "\treset the above items if necessary.\n");
	process_command("0<\r", 0);
	process_command("E2]\r08\r", 0);
	process_command("E1]\r1\r", 0);
	process_command(buf, 0);
	if (mywrite(promfd, "I\r", 2, 0) != 2)
	    error("cannot write to prom (%d)\n", errno);
	break;
    case 'B':
	/* jedec needs CRMOD to make able's checksum correct */
#ifdef BSD
	ioctl(promfd, TIOCGETP, (char *)&arg);
	arg.sg_flags |= CRMOD;
	ioctl(promfd, TIOCSETP, (char *)&arg);
#endif
#ifdef SYSV
	/*
	 * According to the BSD documentation, CRMOD converts input cr's
	 * to nl and output nl to cr-lf.  The equivalent of that in SysV
	 * is to set the input flag to ICRNL and the output flag to ONLCR
	 */
	ioctl(promfd, TCGETA, &term);
	term.c_iflag |= ICRNL;
	term.c_oflag |= ONLCR;
	ioctl(promfd, TCSETA, &term);
#endif SYSV
	if (mywrite(promfd, "EB]\r", 4, 0) != 4)
	    error("cannot write to prom (%d)\n", errno);
	break;
    } /* switch */


    /* get number of 1024 blocks in file for count display */
    fstat(fileno(stdinput), &statb);
    count = statb.st_size / 1024;

    /* set flag once to see if output going to tty */
    isatty_flag = isatty(fileno(stdout));

#ifndef BAD
    while (fgets(line, sizeof(line), stdinput) != NULL) {
	length = strlen(line);
	if (line[length-1] != '\n')
	    error("line too long (> 1024)\n");

again:
	if (mywrite(promfd, line, length, 1) != length) {
	    
	    fprintf(stderr, "\nproblems writing to prom (%d,%d)\n", result, errno);
	    reopen_tty();
	    process_command("\033", 0);
	    switch (*command) {
	    case 'I':
		if (mywrite(promfd, "I\r", 2, 0) != 2)
		    error("cannot write to prom (%d)\n", errno);
		break;
	    case 'B':
		if (mywrite(promfd, "EB]\r", 4, 0) != 4)
		    error("cannot write to prom (%d)\n", errno);
		break;
	    } /* switch */
	    goto again;
	} /* if */

	/* update count for anxious user */
	if (isatty_flag) {
	    fprintf(stdout, "%d   \r", nolines--); fflush(stdout);
	} /* if */
    } /* while */

#else
    while ((length = fread(buf, 1, sizeof(buf), stdinput)) > 0) {

	if (mywrite(promfd, buf, length, 1) != length)
	    error("cannot write to prom (%d,%d)\n", result, errno);

	/* update count for anxious user */
	if (isatty_flag)
	    fprintf(stdout, "%d   \r", count--); fflush(stdout);
    } /* while */
#endif


#ifdef BSD
    /* reset tty */
    ioctl(promfd, TIOCGETP, (char *)&arg);
    arg.sg_flags = CBREAK|DECCTQ|LITOUT|ANYP;
    ioctl(promfd, TIOCSETP, (char *)&arg);
#endif
/*
#ifdef SYSV
	ioctl(fncomm, TCGETA, &term);
	term.c_iflag = IXON | IXOFF;
	term.c_oflag = 0;
	ioctl(fncomm, TCSETA, &term);
#endif
*/
	
    /* close file and return success */
    fclose(stdinput);
    fprintf(stdout, "\n"); fflush(stdout);
    return 1;
} /* file_to_ram */


error(f, a, b, c)
{
    /* fatal error, print and exit */
    fprintf(stderr, "\n");
    fprintf(stderr, f, a, b, c);
    exit(1);
} /* error */


upper_case(buf)
char	*buf;
{
    /* used to convert sprintf hex numbers to upper case */
    for(; *buf; buf++) {
	if (*buf > 'Z')
	    *buf -= ('a' - 'A');
    } /* for */
} /* upper_case */


help()
{
    /* print out basic help info */
    fprintf(stdout, "(^char control caracter, h hex digit- upper case only, d decimal digit)\n");
    fprintf(stdout, "COMMAND		ACTION\n");
    fprintf(stdout, "-------		----------------------------------------\n");
    fprintf(stdout, "-Ifilename	copy file to RAM\n");
    fprintf(stdout, "-Bfilename	copy jedec file to RAM\n");
    fprintf(stdout, "-E		exit crc upon any failure\n");
    fprintf(stdout, "-Ddd		set block limit to (# bytes from last -I) div dd\n");
    fprintf(stdout, "-q		quit\n");
    fprintf(stdout, "--		comment\n");
    fprintf(stdout, "^C		get back to crc prompt\n");
    fprintf(stdout, "<esc>		abort current operation\n");
    fprintf(stdout, "E2]^V^Mdd	set word size in bits\n");
    fprintf(stdout, "E1]^V^Md	set set size -- how many sets of proms?\n");
    fprintf(stdout, "hhhhh:		set beginning device address\n");
    fprintf(stdout, "hhhhh;		set block limit in words\n");
    fprintf(stdout, "hhhhh<		set RAM beginning address\n");
    fprintf(stdout, "hhhh@		select family/pinout code (our proms DDA4)\n");
    fprintf(stdout, "hhA		select translation format (srec is 87)\n");
    fprintf(stdout, "P		program devices from RAM\n");
    fprintf(stdout, "S		return checksum for data in RAM\n");
    fprintf(stdout, "T		do illegal bit test on devices\n");

    fprintf(stdout, "HH]		select function (see E2, E1 above)\n");
} /* help */


void
handler()
{
#ifdef BSD
    struct sgttyb	arg;

    /* clear CRMOD */
    ioctl(promfd, TIOCGETP, (char *)&arg);
    arg.sg_flags &= ~CRMOD;
    ioctl(promfd, TIOCSETP, (char *)&arg);
#endif
#ifdef SYSV
    struct termio term;

    ioctl(promfd, TCGETA, &term);
    term.c_iflag &= ~ICRNL;
    term.c_oflag &= ~ONLCR;
    ioctl(promfd, TCSETA, &term);
#endif
	
    /* if we get a ^C longjump if input is from tty, else exit(1) */
    if (isatty(fileno(stdin)))
	longjmp(jumpbuf);

    error("Exiting, Got ^C with redirected input from a file\n");
} /* handler */


mywrite(fd, buf, nbytes, return_on_error)
int	fd;
char	*buf;
int	nbytes;
int	return_on_error;
{
    long	writefds = 1<<fd;
    long	base = 0;
    long	result;
    long	length = nbytes;
    long	nfound;

#ifdef BSD
    ioctl(fd, TIOCFLUSH);
#endif BSD
#ifdef SYSV
    ioctl(fd, TCFLSH,0);
#endif SYSV
write_again:
    length = nbytes;
    base = 0;
    nfound = select(fd+1, 0, &writefds, 0, 0);
    if (nfound <= 0 || writefds == 0)
	return -1;

    /* write in loop until data-io can eat it all */
    errno = 0;
    if ((result=write(promfd, buf + base, length)) != length) {
	if (errno == EWOULDBLOCK || errno == 0) {
	    if (result > 0) {
		length -= result;
		base += result;
	    } /* if */
	    goto write_again;
	} else if (result < 0 && errno == EIO) {
	    if (return_on_error)
		return result;
	    reopen_tty();
	    process_command("\033", 0);
	    goto write_again;
	} /* if */
	return -1;
    } /* if */
    reopen = 0;
    return nbytes;
} /* mywrite */

reopen_tty()
{
    if (reopen > 3)
	error("got unfixable io error writing to port\n");
    reopen++;
    close (promfd);
    promfd = open(ttyfile, O_RDWR|O_NDELAY);
    if (promfd < 0)
	error("cannot open file %s (%d)\n", ttyfile, errno);
    ttysetup(promfd, B4800);
} /* reopen_tty */


myread(fd, buf, nbytes)
int fd, nbytes;
char *buf;
{
    int		result;
    int		base;

    base = 0;
    errno = 0;
    do {
	result = read(fd, buf+base, nbytes);
	if (result == 0 || (result < 0 && errno == EWOULDBLOCK)) {
	    continue;
	} else if (result < 0 && errno == EIO) {
	    reopen_tty();
	    continue;
	} /* if */
	if (result < nbytes) {
	    base += result;
	    nbytes -= result;
	} else {
	    break;
	} /* if */
    } while (nbytes > 0);
    reopen = 0;
} /* myread */
