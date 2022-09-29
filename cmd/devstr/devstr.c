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
#ident	"$Header: devstr.c,v 1.1.2.6 90/05/09 15:41:54 wje Exp $"

/*
 * devstr - print device strings
 *
 * Usage: devstr [-a] [-f format] device...
 *
 * Devstr gets the device identification string for each named device
 * and formats it.  The formatting is similar to date(1), in that %-
 * specifiers and \-escapes can be used to print the fields.
 *
 * The following %-specifiers are available:
 *
 *	%f	Filename (name of the device)
 *	%v	Vendor name
 *	%V	Vendor name padded to 8 columns
 *	%p	Product id
 *	%P	Product id padded to 16 columns
 *	%r	Revision
 *	%R	Revision padded to 4 columns
 *	%m	Microcode revision
 *	%M	Microcode revision padded to 8 columns
 *	%n	Serial number
 *	%N	Serial number padded to 12 columns
 *	%t	Device Type
 *	%T	Device Type padded to 16 columns
 *
 * The padded items are provided to make the formatted strings look
 * just like the strings given back by the ioctl.
 */

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/gen_ioctl.h>
#include <sys/types.h>
#include <sys/dkio.h>
#include <sys/dvh.h>
#include <sys/socket.h>
#include <bsd/net/if.h>
#include <bsd/net/soioctl.h>

char Vendor[IDENT_VENDOR + 1];
char Product[IDENT_PRODUCT + 1];
char Revision[IDENT_REVISION + 1];
char Micro[IDENT_MICRO + 1];
char Serial[IDENT_SERIAL + 1];
char Dtype[IDENT_TYPE + 1];
int Fd;
extern int errno;

#define DEF_FMT	"%V-%P-%R-%M-%N-%T"
#define ALT_FMT "%f %V-%P-%R-%M-%N-%T"

main(argc, argv)
int argc;
char **argv;
{

	extern int optind;
	int c;
	extern char *optarg;
	char *format;
	struct io_arg io_arg;
	int getctlr = 0;

	format = DEF_FMT;

	while ((c = getopt(argc, argv, "f:a")) != EOF) {
		switch(c) {

		case 'f':
			format = optarg;
			break;

		case 'a':
			format = ALT_FMT;
			break;
		
		default:
			usage();
			exit(2);
		}
	}

	if (optind == argc) {
		usage();
		exit(2);
	}

	while (optind < argc) {
	    if((Fd = open(argv[optind], O_RDONLY|O_SYNC)) < 0){
		/* 
		 * Could be ethernet interface
		 */
		get_enet(argv[optind]);
	    }else{
		if (get_id(argv[optind])) {
			print_str(argv[optind], format);
		}
	        get_ctlr(argv[optind]);
		close(Fd);
	    }
		optind++;
	}
	exit(0);
}

usage()
{
	fprintf(stderr, "devstr: usage: devstr [-a] [-f format] device...\n");
}

/*
 * Format the information strings using the given format.
 */

print_str(name, fmt)
	char *name;
	char *fmt;
{

	while (*fmt) {
		switch (*fmt) {

		case '\\':
			fmt++;
			switch (*fmt) {

			case 'n':
				putchar('\n');
				break;

			case 'f':
				putchar('\f');
				break;

			case 'r':
				putchar('\r');
				break;

			case 'b':
				putchar('\b');
				break;

			case 't':
				putchar('\t');
				break;

			default:
				putchar(*fmt);
				break;

			}

			fmt++;
			continue;

		case '%':
			fmt++;
			break;

		default:
			putchar(*fmt);
			fmt++;
			continue;
		}

		switch (*fmt) {

		case '%':
			putchar(*fmt);
			break;

		case 'f':
			printf("%s", name);
			break;

		case 'V':
			printf("%-8.8s", Vendor);
			break;

		case 'v':
			printf("%s", Vendor);
			break;

		case 'P':
			printf("%-16.16s", Product);
			break;

		case 'p':
			printf("%s", Product);
			break;

		case 'R':
			printf("%-4.4s", Revision);
			break;

		case 'r':
			printf("%s", Revision);
			break;

		case 'M':
			printf("%-8.8s", Micro);
			break;

		case 'm':
			printf("%s", Micro);
			break;

		case 'N':
			printf("%-12.12s", Serial);
			break;

		case 'n':
			printf("%s", Serial);
			break;

		case 'T':
			printf("%-16.16s", Dtype);
			break;

		case 't':
			printf("%s", Dtype);
			break;

		default:
			putchar('%');
			putchar(*fmt);
			break;
		}
		fmt++;
	}
	putchar('\n');
}

/*
 * Get the device id information.
 */

int
get_id(name)
	char *name;
{
	struct gioctl gioc;

	if(ioctl(Fd, GIOCPRSTR, &gioc) < 0){
	    if( errno != ENOTSUP){
		perror(name);
	    }
	    return 0;
	}

	breakup(gioc.gi_ident);

	return 1;
}
/*
 * Break the device string into its components.
 */

typedef struct inq_component {
	char *comp;
	int size;
}INQ_COMP;

INQ_COMP inq_component[] = {
	{Vendor,IDENT_VENDOR},
	{Product,IDENT_PRODUCT},
	{Revision,IDENT_REVISION},
	{Micro,IDENT_MICRO},
	{Serial,IDENT_SERIAL},
	{Dtype,IDENT_TYPE}
};


breakup(str)
char *str;
{
    register int i;
    register INQ_COMP *cmp;
    char *place;
	
    for (i = 0; i < sizeof(inq_component)/sizeof(INQ_COMP); ++i) {
	cmp = &inq_component[i];
	strncpy(cmp->comp, str, cmp->size);
	place = &cmp->comp[cmp->size-1];
	while (*place == ' ') {
		place--;
	}
	place++;
	*place = '\0';
	str += (cmp->size + 1);
    }
}

/*
 * Liberally adapted from ifconfig.c
 */
get_enet(name)
	char *name;
{
	int s;
	struct io_arg io_arg;
	struct ctlr_info ct;
	struct ifreq ifr;
	char buffer[SIOCTLRSIZE];

	int af = AF_INET;

        s = socket(af, SOCK_DGRAM, 0);
        if (s < 0) {
		printf("%s : Not a file or socket\n", name);
                usage();
        }

	strncpy( ifr.ifr_name, name, sizeof(ifr.ifr_name));
	ifr.ifr_data = buffer;
        if (ioctl(s, SIOCGCTLR, &ifr) < 0) {
                perror(name);
                exit(1);
        }
	puts(buffer);

}
/*
 * Get the controller info
 */

int
get_ctlr(name)
	char *name;
{
	int fd;
	struct io_arg io_arg;
	struct ctlr_info ct;

	io_arg.memaddr = (unsigned long)&ct;
	io_arg.datasz = sizeof( struct ctlr_info);
	if(ioctl(Fd, DIOCGETCTLR, &io_arg) < 0){
	    if( io_arg.retval == DIOC_BADSIZE){
		printf("Bad argument size passed for DIOCGETCTLR\n");
	    }
	    perror(name);
	    return;
	}
	puts(ct.ci_type);

	return;
}
