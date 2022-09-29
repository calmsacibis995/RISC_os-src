#ident "$Header: sdownload.c,v 1.3.8.1 90/07/18 17:14:40 huang Exp $"
/*	%Q%	%I%	%M%	*/

/*
 * Copyright 1987 by MIPS Computer Systems, Inc.
 */

/* This program will download s3rec files from SYSV or BSD to MIPS prom
 *	monitor. On target machine type:
 *		>> setenv rbaud 9600
 *		>> enable tty(1)
 *		>> sload tty(1)
 * On development machine type:
 *		% sdownload /dev/ttyxx < s3rec.file
 * where /dev/ttyxx is a rs232 line port hooked from the development
 * machine to ttyb on the target.
 */

#include <sys/param.h>
#ifndef BSD
#define SYSV
#endif BSD

#include <stdio.h>
#include <fcntl.h>

#ifdef SYSV
#include <termio.h>
#define TIOCGETP TCGETA
#define TIOCSETP TCSETA
#else BSD
#include <sgtty.h>
#define termio sgttyb
struct tchars otchars;
#endif SYSV

#define DIGIT(c)        ((c)>'9'?((c)>='a'?(c)-'a':(c)-'A')+10:(c)-'0')

main(argc, argv) 
int argc; char *argv[];
{
	char buf[256];
	char c, c1, c2, *cptr, *wptr;
	int i = 0, fd, line = 0, do_binary = 0, len;
        struct termio sg, osg;

	/* loop through arguments */
	for (i = 1; i < argc; i++) {

	    if (*argv[i] == '-') {
		/*
		 *  -<arg>
		 */
		switch(*(argv[i]+1)) {
		    case 'b':
			do_binary = 1;
			break;
		    default:
			printf("error: unknown argument: -%s \n", *(argv[i]+1));
			exit(1);
		} /* switch */
	    } else {
		/*
		 *  <ttyname>
		 */
		printf("Device: %s\n",argv[i]);

		/* open device */
		fd = open(argv[i],O_RDWR);
		if (fd == -1) {
		    printf("error: Device %s could not be opened\n",argv[i]);
		    exit(-1);
		}
	    } /* else */

	} /* for each argument */

	/* set device characteristics */
	if (ioctl(fd, TIOCGETP, &osg) == -1) {
		fprintf(stderr,"ioctl(TIOCGETP) failed\n");
		perror("sio_init");
		return(0);
	}
	sg = osg;
#ifdef SYSV
	sg.c_iflag &= ~(BRKINT|IGNPAR);
	sg.c_lflag &= ~(ISIG|ICANON|ECHO);
	sg.c_oflag  = 0;
	sg.c_cc[VEOF] = 1;
	sg.c_cc[VEOL] = 0;
#else BSD
	sg.sg_flags = RAW;
	sg.sg_ispeed = sg.sg_ospeed = B9600;
#endif
	if (ioctl(fd, TIOCSETP, &sg) == -1) {
		fprintf(stderr,"ioctl(TIOCSETP) failed\n");
		perror("sio_init");
		return(0);
	}

	/* write s3rec header record */
	buf[0] = 'S';
	buf[1] = '0';
	if (do_binary) {
	    buf[2] = 0x00;
	    buf[3] = 0xFF;
	    len    = 4;
	} else {
	    buf[2] = '0';
	    buf[3] = '0';
	    buf[4] = 'F';
	    buf[5] = 'F';
	    len    = 6;
	}
	if (write(fd,buf,len) != len) {
		perror("bad write initial packet to device: ");
		goto restore;
	}

	/* read ack */
	if (read(fd,&c,1) != 1) {
		perror("bad read from device: ");
		goto restore;
	}
	if (c != 0x6) {
		printf("Bad ack on S0 packet\n");
		goto restore;
	}

	/* main loop:
	 *	read line from input
	 *	write line out to /dev/ttyxx
	 *	read ack back
	 */
	while (gets(buf)) {

		line++;		/* this is our current line number */

		/* write line to /dev/ttyxx */
		if (do_binary) {
		    /*
		     *  Binary -- write the first two characters in ascii,
		     *		  then convert the rest to binary
		     */
		    wptr = cptr = buf+2;
		    len  = 2;
		    while( (c1 = *cptr++) != '\0' ) {
			c2 = *cptr++;
			*wptr++ = (DIGIT(c1 & 0x7F) << 4) | DIGIT(c2 & 0x7F);
			len++;
		    } /* while */
		} else {
		    /*
		     *  Ascii -- just write the input record as-is
		     */
		    len = strlen(buf);
		}
		if (write(fd,buf,len) != len) {
			perror("bad write of line %d to device: ",line);
			goto restore;
		}

		/* read ack */
		if (read(fd,&c,1) != 1) {
			perror("bad read from device: ");
			goto restore;
		}
		c &= 0x7f;	/* mask out parity */
		if (c != 0x6) {
			fprintf(stderr, "Bad acknowledgment line %d \n", line);
			goto restore;
		}

		/* status line */
		if ((line & 0x1F) == 0) {	/* every so often... */
			printf("%d   \r", line);
			fflush(stdout);
		}
	}
	printf("Done total line %d\n",line);
restore:
	/* restore tty state */
	/* we no longer restore the tty state, as certian race conditions */
	/* seem to re-enable echo (if it was set before) before the final */
	/* output from sload has gotten to sdownload, and this causes no  */
	/* end of trouble */
	/*
	if (ioctl(fd, TIOCSETP, &osg) == -1) {
		fprintf(stderr,"ioctl(TIOCSETP) failed\n");
		perror("sio_init");
		return(0);
	}
	*/
	close(fd);
	printf("\n");
}
