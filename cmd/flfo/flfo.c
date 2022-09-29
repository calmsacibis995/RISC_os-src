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
#ident	"$Header: flfo.c,v 1.2.1.2 90/05/09 15:51:13 wje Exp $"
/*
 * $Header: flfo.c,v 1.2.1.2 90/05/09 15:51:13 wje Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/buf.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/dvh.h>
#include <sys/scsi.h>
#include <sys/flio.h>

struct io_arg		io_arg;
struct flop_msel	msel;
struct volume_header 	vh;
char			*name;

main(argc, argv)
	int argc;
	char **argv;
{
	struct stat stb;
	int fd, ilv = 0;
	int iflag = 0, fflag = 0, vflag = 0, mflag = 0;


	name = *argv;
	argc--, argv++;
	while (argc > 1 && **argv == '-') {
		(*argv)++;
		while (**argv) switch (*(*argv)++) {
		case 'i':	/* set interleave */
			iflag++;
			break;
		case 'f':	/* do format */
			fflag++;
			break;
		case 'v':	/* set interleave */
			vflag++;
			break;
		case 'm':	/* get/set mode parameters */
			mflag++;
			break;
		default:
			goto usage;
		}
		argc--; argv++;
	}
	if (argc != iflag + 1)
		goto usage;
	if (iflag) {
		sscanf(*argv, "%d", &ilv);
		argc--; argv++;
	}
	if ((fd = open(*argv, O_RDWR, 0)) < 0) {
		fprintf(stderr, "%s: cannot open", *argv);
		perror(" ");
		exit (1);
	}
	if (fstat(fd, &stb) < 0) {
		fprintf(stderr, "%s: cannot stat", *argv);
		perror(" ");
		exit (1);
	}
	if ((stb.st_mode&S_IFMT) != S_IFCHR) {
		fprintf(stderr, "%s: not a char special file\n", *argv);
		exit (1);
	}
	if (mflag && get_set_parms(fd))
		exit(1);
	if (fflag && do_format(fd, ilv))
		exit(1);
	if (vflag && write_vh(fd))
		exit(1);
	exit(0);
usage:
	fprintf(stderr, "Usage: %s [-[m][f][v][i ilv]] floppy_device\n", name);
	exit(1);
}

#define GET_PARM(str, var, min, max, conv)	while (1) {		\
	fprintf(stderr, "%s (%d - %d): ", str, min, max);		\
	gets(buf);							\
	sscanf(buf, "%i\n", &inp);					\
	if (inp < min || inp > max)					\
		fprintf(stderr, "%d is outside valid range\n", inp);	\
	else {								\
		var = (conv )inp;					\
		break;							\
	}								\
}

#define SHOW_PARM(str, var)	fprintf(stderr, "%s: %d\n", str, var)

get_set_parms(fd)
{
	char			buf[20];
	struct pg_flop		*pg = &msel.msel_desc.desc_pg;
	int			inp;

	msel.msel_desc.desc_code = PD_FLOP;
	msel.msel_desc.desc_len = sizeof(struct pg_flop);
	GET_PARM("Number of cylinders per disk",
		pg->flop_ncyl,		1, 0xFFFF, u_short);
	GET_PARM("Number of bytes per sector",
		pg->flop_nbyte,		1, 0xFFFF, u_short);
	GET_PARM("Number of sectors per track",
		pg->flop_nsec,		1, 0xFFFF, u_short);
	GET_PARM("Number of drive heads (surfaces)",
		pg->flop_nhead,		1, 0xFF, u_char);
	GET_PARM("Transfer rate",
		pg->flop_xfer,		1, 0xFF, u_char);
	GET_PARM("Normal gap length",
		pg->flop_norm_gap,	1, 0xFF, u_char);
	GET_PARM("Format gap length",
		pg->flop_fmt_gap,	1, 0xFF, u_char);
	GET_PARM("Motor on delay",
		pg->flop_mon,		1, 0xFF, u_char);
	GET_PARM("Motor off delay",
		pg->flop_moff,		1, 0xFF, u_char);
	GET_PARM("Head settle delay in ms",
		pg->flop_hsd,		1, 0xFF, u_char);
	GET_PARM("head step rate in ms",
		pg->flop_step_rate,	1, 0xFF, u_char);
	GET_PARM("head load time in 2ms units",
		pg->flop_hlt,		1, 0x7F, u_char);
	GET_PARM("head unload time in 16ms units",
		pg->flop_hut,		1, 0xF, u_char);
	GET_PARM("MFM encoding (non-zero)",
		pg->flop_mfm,		0, 0x1, u_char);
	GET_PARM("High capacity drive (non-zero)",
		pg->flop_hcap,		0, 0x1, u_char);

	fprintf(stderr, "\nSELECTED PARAMETERS ARE:\n");
	SHOW_PARM("Number of cylinders per disk",	pg->flop_ncyl);
	SHOW_PARM("Number of bytes per sector",		pg->flop_nbyte);
	SHOW_PARM("Number of sectors per track",	pg->flop_nsec);
	SHOW_PARM("Number of drive heads (surfaces)",	pg->flop_nhead);
	SHOW_PARM("Transfer rate",			pg->flop_xfer);
	SHOW_PARM("Normal gap length",			pg->flop_norm_gap);
	SHOW_PARM("Format gap length",			pg->flop_fmt_gap);
	SHOW_PARM("Motor on delay",			pg->flop_mon);
	SHOW_PARM("Motor off delay",			pg->flop_moff);
	SHOW_PARM("Head settle delay in ms",		pg->flop_hsd);
	SHOW_PARM("head step rate in ms",		pg->flop_step_rate);
	SHOW_PARM("head load time in 2ms units",	pg->flop_hlt);
	SHOW_PARM("head unload time in 16ms units",	pg->flop_hut);
	SHOW_PARM("MFM encoding (non-zero)",		pg->flop_mfm);
	SHOW_PARM("High capacity drive (non-zero)",	pg->flop_hcap);

	fprintf(stderr, "\nsetting parameters...");
	io_arg.memaddr = (unsigned long )&msel;
	if (ioctl(fd, FLIOCMODSLCT, &io_arg) < 0) {
		fprintf(stderr, "%s: mode select failed", name);
		perror(" ");
		return (1);
	}
	fprintf(stderr, "done\n");
	return(0);
}

do_format(fd, ilv)
{
	fprintf(stderr, "formatting...");
	io_arg.memaddr = (unsigned long )&ilv;
	if (ioctl(fd, FLIOCFORMAT, &io_arg) < 0) {
		fprintf(stderr, "%s: format failed", name);
		perror(" ");
		return (1);
	}
	fprintf(stderr, "done\n");
	return(0);
}

write_vh(fd)
{
	struct volume_header 	*vhp = &vh;
	int			*ip = (int *)vhp;
	int			csum = 0;
	struct pg_flop		*pg = &msel.msel_desc.desc_pg;
	int			i, cylsize, dksize;

	fprintf(stderr, "getting parameters...");
	io_arg.memaddr = (unsigned long )&msel;
	if (ioctl(fd, FLIOCMODSNS, &io_arg) < 0) {
		fprintf(stderr, "%s: mode sense failed", name);
		perror(" ");
		return (1);
	}
	fprintf(stderr, "done\n");

	cylsize = pg->flop_nhead * pg->flop_nsec;
	dksize = pg->flop_ncyl * cylsize;
	for (i=0; i<sizeof(vh); i++)
		*((char *)vhp + i) = 0;
	vhp->vh_magic = VHMAGIC;
	for (i=0; i<8; i++)
		vhp->vh_pt[i].pt_type = PTYPE_BSD42;
	vhp->vh_pt[0].pt_nblks = vhp->vh_pt[2].pt_nblks = dksize - cylsize;
	vhp->vh_pt[0].pt_firstlbn = vhp->vh_pt[2].pt_firstlbn = cylsize;
	for (i=8; i<16; i++)
		vhp->vh_pt[i].pt_type = -1;
	vhp->vh_pt[8].pt_nblks = cylsize;
	vhp->vh_pt[8].pt_type = PTYPE_VOLHDR;
	vhp->vh_pt[10].pt_nblks = dksize;
	vhp->vh_pt[10].pt_type = PTYPE_VOLUME;

	while (ip < (int *)(vhp + 1))
		csum += *ip++;
	vhp->vh_csum = -csum;

	fprintf(stderr, "'c' partition contains %d sectors (%d x %d x %d)\n",
		dksize - cylsize,
		pg->flop_nsec, pg->flop_nhead, pg->flop_ncyl - 1);
	fprintf(stderr, "writing volume header...");
	for (i = 0; i < pg->flop_nhead; i++)
	    if (lseek(fd, i * pg->flop_nsec * pg->flop_nbyte, 0) < 0 ||
	        write(fd, vhp, sizeof(vh)) != sizeof(vh)) {
		    fprintf(stderr, "%s: failed", name);
		    perror(" ");
		    return (1);
	    }
	fprintf(stderr, "done\n");
	return(0);
}
