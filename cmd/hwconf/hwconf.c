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
#ident	"$Header: hwconf.c,v 1.3.1.6.1.4.1.6 91/01/14 19:27:21 beacker Exp $"

#include <stdio.h>
#include <sys/types.h>
#define imp_tbl bsd43_imp_tbl
#include <machine/hwconf.h>
#undef imp_tbl
#include <sysv/sys/cpu_board.h>
#include <sysv/sys/iop.h>
#include <sysv/sys/flio.h>

char *rimp_name();
char *imp_name();
char *ext_imp_name();

struct imp_tbl {
	char *it_name;
	unsigned it_imp;
	unsigned it_machtype;
};

struct rimp_tbl {
	char *it_name;
	unsigned it_imp;
	unsigned it_majrev;
};

struct rimp_tbl cpu_imp_tbl[] = {
	{ "MIPS R2000 Processor Chip",			0, 0 },
	{ "MIPS R2000 Processor Chip",			1, 0 },
	{ "MIPS R2000A Processor Chip",			2, 1 },
	{ "MIPS R3000 Processor Chip",			2, 2 },
	{ "MIPS R3000A Processor Chip",			2, 3 },
	{ "MIPS R6000 Processor Chip",			3, 0 },
	{ 0,						0, 0 }
};

struct rimp_tbl fp_imp_tbl[] = {
	{ "MIPS R2360 Floating Point Board",		1, 0 },
	{ "MIPS R2010 VLSI Floating Point Chip",	2, 0 },
	{ "MIPS R2010A VLSI Floating Point Chip",	3, 1 },
	{ "MIPS R3010 VLSI Floating Point Chip",	3, 2 },
	{ "MIPS R3010A VLSI Floating Point Chip",	3, 3 },
	{ "MIPS R3010A VLSI Floating Point Chip",	3, 4 },
	{ "MIPS R6010 Floating Point Chip",		4, 0 },
	{ 0,						0, 0 }
};

struct imp_tbl board_imp_tbl[] = {
  	{ "MIPS R2300 Cpu Board",			0, BRDTYPE_R2300 },
	{ "MIPS R2400 Cpu Board",			0, BRDTYPE_R2400 },
	{ "MIPS R2600 Cpu Board",			0, BRDTYPE_R2600 },
	{ "MIPS R2800 Cpu Board",			0, BRDTYPE_R2800 },
	{ "MIPS R3200 Cpu Board",			0, BRDTYPE_R3200 },
	{ "MIPS RB6130 Cpu Board",			0, BRDTYPE_R6300 },
	{ "MIPS I2000 Cpu Board",			0, BRDTYPE_I2000 },
	{ "MIPS I2000S Cpu Board",			0, BRDTYPE_I2000S },
	{ "MIPS R3030 Cpu Board",			0, BRDTYPE_R3030 },
	{ "MIPS RB3125 Cpu Board",			0, BRDTYPE_RB3125 },
	{ "MIPS RB3133 Cpu Board",			0, BRDTYPE_RB3133 },
	{ 0,						0, 0 }
};

struct hw_config Conf;

main(argc, argv)
int argc;
char **argv;
{
	struct promenv p;
	int sflag, gflag;

	get_config();

	argc--, argv++;
	while (argc > 0 && **argv == '-') {
		argc--;
		switch (argv[0][1]) {

		case 'g':
			gflag++;
			break;

		case 's':
			if (argc < 2) {	/* make sure ptrs are there */
				usage();
				exit(1);
			}
			strncpy(p.name, *++argv, ENV_MAXLEN);
			strncpy(p.value, *++argv, ENV_MAXLEN);
			if (set_config(&p))
				sflag++;
			else
				fprintf(stderr, "%s not valid\n", p.name);
			argc -= 2;
			break;

		default:
			usage();
			exit(1);
		}
		argv++;
	}
	if (sflag) {
		if (hwconf(HWCONF_SET, &Conf) < 0) {
			perror("hwconf");
			exit(1);
		}
		if (gflag)
			get_config();
	}
	print_config();
	exit(0);
}

usage()
{
	fprintf(stderr, "usage: hwconf [-g] [-s promenv_name promenv_value]\n");
}

set_config(p)
struct promenv *p;
{
	register int i;

	for (i=0; i < ENV_ENTRIES; i++) {
		if (!strncmp(p->name, Conf.promenv[i].name, ENV_MAXLEN)) {
			strncpy(Conf.promenv[i].value, p->value, ENV_MAXLEN);
			return (1);
		}
	}
	return (0);
}

get_config()
{
	if (hwconf(HWCONF_GET, &Conf) < 0) {
		perror("hwconf");
		exit(1);
	}
}

get_floppy()
{
int f;
#define FLOPPY "/dev/rfd/fd0t01vol"

	if((f = open(FLOPPY,0)) == -1)
		return(1);
	if(ioctl(f,FLIOC_SENSE_DRIVE,0) == -1)
		return(1);
	printf("\tfloppy drive present\n");
	return(0);
}

print_config()
{
	register int i;

	printf("%s, Revision %d.%d\n",
	    rimp_name(Conf.cpu_processor, cpu_imp_tbl),
	    Conf.cpu_processor.ri_majrev, Conf.cpu_processor.ri_minrev);
	if (Conf.fpu_processor.ri_uint) {
		printf("%s Revision: %d.%d\n",
		    rimp_name(Conf.fpu_processor, fp_imp_tbl),
		    Conf.fpu_processor.ri_majrev, 
		    Conf.fpu_processor.ri_minrev);
	}

	printf("%s, Revision %x.%x, ", 
		ext_imp_name(0,board_imp_tbl),
		(Conf.cpubd_rev & 0xf0) >> 4, 
		Conf.cpubd_rev & 0xf);
	printf("Serial Number %c%c%c%c%c\n", Conf.cpubd_snum[0],
		Conf.cpubd_snum[1], Conf.cpubd_snum[2],
		Conf.cpubd_snum[3], Conf.cpubd_snum[4]);
	printf("\ticache size = %d, dcache size = %d\n", 
		Conf.icache_size, Conf.dcache_size);

	printf("CPU board configuration:\n");
	switch (Conf.cpubd_type) {
	case BRDTYPE_R2300:
	case BRDTYPE_R2600:
	case BRDTYPE_R2800:
		if (Conf.cpubd_config & CONFIG_NOCP1)
			printf("\tcoprocessor 1 not present\n");
		else
			printf("\tcoprocessor 1 present\n");
		if (Conf.cpubd_config & CONFIG_NOCP2)
			printf("\tcoprocessor 2 not present\n");
		else
			printf("\tcoprocessor 2 present\n");
		if (Conf.cpubd_config & CONFIG_POWERUP)
			printf("\tcold start\n");
		else
			printf("\twarm start\n");
		if (Conf.cpubd_config & CONFIG_VMEMEM)
			printf("\tvme memory\n");
		else
			printf("\tprivate memory\n");
		break;
	case BRDTYPE_R2400:
		switch (Conf.cpubd_rev) {
		case REV_R2400_16:
			printf("\t16 MHZ\n");
			break;
		case REV_R2400_12_5:
			printf("\t12.5 MHZ\n");
			break;
		default:
			break;
		};
		if (Conf.cpubd_config & SCR_NOFPP)
			printf("\tcoprocessor 1 not present\n");
		else
			printf("\tcoprocessor 1 present\n");
		printf("\tcoprocessor 2 not present\n");
		if (Conf.cpubd_config & SCR_COLDSTART)
			printf("\tcold start\n");
		else
			printf("\twarm start\n");
		if (Conf.cpubd_config & SCR_NOBOOTLOCK)
			printf("\tkey switch is not locked\n");
		else
			printf("\tkey switch is locked\n");
		break;
	case BRDTYPE_R3200:
		switch (Conf.cpubd_rev) {
		case REV_R3200_20:
			printf("\t20 MHZ\n");
			break;
		case REV_R3200_25:
			printf("\t25 MHZ\n");
			break;
		default:
			break;
		};
		/* FALL THRU */
	case BRDTYPE_RB3125:
	case BRDTYPE_RB3133:
		if (Conf.cpubd_config & CR_FPPRES)
			printf("\tcoprocessor 1 not present\n");
		else
			printf("\tcoprocessor 1 present\n");
		printf("\tcoprocessor 2 not present\n");
		if (Conf.cpubd_config & P_NEWSCC)
			printf("\tserial controller 85C130\n");
		else
			printf("\tserial controller 85C30\n");
		if (Conf.cpubd_config & CR_COLDSTART)
			printf("\tcold start\n");
		else
			printf("\twarm start\n");
		break;
	case BRDTYPE_I2000:
	case BRDTYPE_I2000S:
		if (Conf.cpubd_config & FPU_Not_Present)
			printf("\tcoprocessor 1 not present\n");
		else
			printf("\tcoprocessor 1 present\n");
		printf("\tcoprocessor 2 not present\n");
		if (Conf.cpubd_config & DBG_Not_Present)
			printf("\tdebug board not present\n");
		else
			printf("\tdebug board present\n");
		if (Conf.cpubd_config & Video_Not_Present)
			printf("\tvideo board not present\n");
		else
			printf("\tvideo board present\n");
		break;
	case BRDTYPE_R6300:
		if (Conf.cpubd_config & CONFIG_NOCP1)
			printf("\tcoprocessor 1 not present\n");
		else
			printf("\tcoprocessor 1 present\n");
		break;
	case BRDTYPE_R3030:
		printf("\tsystem clock rate is %d MHz\n",
			((unsigned)Conf.cpubd_config & P_SPEED) >> P_SPEED_SHIFT);
		printf("\tsystem memory size is %d Mbytes\n",
			((((unsigned)Conf.cpubd_config & P_MEMSIZE) >> P_MEM_SHIFT) + 1) * 8);
		if (Conf.cpubd_config & P_NO_FPU)
			printf("\tcoprocessor 1 not present\n");
		else
			printf("\tcoprocessor 1 present\n");
		printf("\tcoprocessor 2 not present\n");
		switch (Conf.cpubd_config & P_DIGI) {
		case P_DIG8:
			printf("\t8 port digiboard present\n");
			break;
		case P_DIG16:
			printf("\t16 port digiboard present\n");
			break;
		default:
			printf("\tdigiboard not present\n");
			break;
		}
		if(get_floppy()) {
			if (Conf.cpubd_config & P_FLOPPY)
				printf("\tfloppy disk present\n");
			else
				printf("\tfloppy disk not present\n");
		}
		if (Conf.cpubd_config & P_MONO)
			printf("\tmonochrome display enabled\n");
		else
			printf("\tmonochrome display not enabled\n");
		if (Conf.cpubd_config & P_COLOUR)
			printf("\tcolor display enabled\n");
		else
			printf("\tcolor display not enabled\n");
		if (Conf.cpubd_config & P_4SIMMS)
			printf("\tmemory configured for 4 Mb simms\n");
		else
			printf("\tmemory configured for 1 Mb simms\n");
		if (Conf.cpubd_config & P_NEWSCC)
			printf("\tserial controller 85C130\n");
		else
			printf("\tserial controller 85C30\n");
		break;
	default:
		if (Conf.fpu_processor.ri_uint == 0)
			printf("\tcoprocessor 1 not present\n");
		else
			printf("\tcoprocessor 1 present\n");
		break;
	};

	printf("Prom Environment:\n");
	for (i=0; i < ENV_ENTRIES; i++) {
		printf("\t%s = %s\n", Conf.promenv[i].name, 
				Conf.promenv[i].value);
	}
}

char *
imp_name(ri, itp)
struct rev_id ri;
struct imp_tbl *itp;
{
	for (; itp->it_name; itp++)
		if ( (itp->it_imp == ri.ri_imp) && 
		   	( (itp->it_machtype == 0) ||
		   	  (itp->it_machtype == Conf.cpubd_type)))
				return(itp->it_name);
	return("Unknown implementation");
}

char *
rimp_name(ri, itp)
struct rev_id ri;
struct rimp_tbl *itp;
{

	for (; itp->it_name; itp++) {
		if (itp->it_imp == ri.ri_imp) {
		   switch (itp->it_majrev) {
		   case 0: 
			   break;
		   case 1:
		   case 2:
		   case 3:
			   itp += (ri.ri_majrev - 1);
			   break;
		   default:
			   return("Unknown implementation");
		   }

		   return(itp->it_name);
		}
         }
	 return("Unknown implementation");
}

/* external imp_name, for externalizing board names for OEM customization*/
char linebuf[100];
char *
ext_imp_name(ri, itp)
struct rev_id ri;
struct imp_tbl *itp;
{
	char * imp_name();
	FILE * namefd;
	char *p,*q;
	int n;

	if ( (namefd = fopen("/usr/lib/board_names","r")) == NULL)
	{
	    return(imp_name(ri,itp));
	}
	while ( fgets(linebuf,sizeof(linebuf),namefd) != NULL)
	{
	    for (p = linebuf;*p;p++)
	    {
		if ( *p == ':' )
		{
		    *p = '\0';	/* terminate the number string */
		    n = atoi(linebuf);
		    if ( n == Conf.cpubd_type)
		    {
			fclose(namefd);
			++p;		/* p now pts to name */
			/* zap the trailing LF */
			for (q=p;*q;q++)
			{
			    if ( *q == '\n' || *q == '\r')
				*q = '\0';
			}
			return (p);
		    }
		}
	    }
	}
	fclose(namefd);
	return("Board Type not listed in /usr/lib/boardnames");
}
