#ident "$Header: commands.c,v 1.8 90/05/31 15:37:19 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * commands.c -- dbgmon commands
 */

#include "sys/types.h"
#include "sys/file.h"
#include "machine/cpu_board.h"
#include "machine/cpu.h"
#include "saio/stringlist.h"
#include "saio/parser.h"
#include "saio/saioctl.h"
#include "prom/entrypt.h"
#include "dbgmon/dbgmon.h"
#include "saio/ctype.h"

extern int machine_type;

#define	streq(x,y)	(strcmp(x,y)==0)

/*
 * cpu register descriptions
 * (probably belong in cpu.h)
 */
extern struct reg_desc sr_desc[], sr_desc_r6000[];
extern struct reg_desc cause_desc[];
extern struct reg_desc tlbhi_desc[];
extern struct reg_desc tlblo_desc[];
extern struct reg_desc tlbinx_desc[];
extern struct reg_desc tlbctxt_desc[];
extern struct reg_desc tlbrand_desc[];
extern struct reg_desc error_desc_r6000[];
extern struct reg_desc tlb_desc_6000[];
extern struct reg_desc ptag_desc_6000[];

static struct reg_table {
	char *rt_name;
	int rt_index;
	struct reg_desc *rt_desc;
} reg_table[] = {
	/* wired zero registers */
	{ "r0",		R_R0,		0 },
	{ "zero",	R_ZERO,		0 },

	/* assembler temps registers */
	{ "r1",		R_R1,		0 },
	{ "at",		R_AT,		0 },

	/* return value registers */
	{ "r2",		R_R2,		0 },
	{ "r3",		R_R3,		0 },
	{ "v0",		R_V0,		0 },
	{ "v1",		R_V1,		0 },

	/* argument registers */
	{ "r4",		R_R4,		0 },
	{ "r5",		R_R5,		0 },
	{ "r6",		R_R6,		0 },
	{ "r7",		R_R7,		0 },
	{ "a0",		R_A0,		0 },
	{ "a1",		R_A1,		0 },
	{ "a2",		R_A2,		0 },
	{ "a3",		R_A3,		0 },

	/* caller saved registers */
	{ "r8",		R_R8,		0 },
	{ "r9",		R_R9,		0 },
	{ "r10",	R_R10,		0 },
	{ "r11",	R_R11,		0 },
	{ "r12",	R_R12,		0 },
	{ "r13",	R_R13,		0 },
	{ "r14",	R_R14,		0 },
	{ "r15",	R_R15,		0 },
	{ "t0",		R_T0,		0 },
	{ "t1",		R_T1,		0 },
	{ "t2",		R_T2,		0 },
	{ "t3",		R_T3,		0 },
	{ "t4",		R_T4,		0 },
	{ "t5",		R_T5,		0 },
	{ "t6",		R_T6,		0 },
	{ "t7",		R_T7,		0 },

	/* callee saved registers */
	{ "r16",	R_R16,		0 },
	{ "r17",	R_R17,		0 },
	{ "r18",	R_R18,		0 },
	{ "r19",	R_R19,		0 },
	{ "r20",	R_R20,		0 },
	{ "r21",	R_R21,		0 },
	{ "r22",	R_R22,		0 },
	{ "r23",	R_R23,		0 },
	{ "s0",		R_S0,		0 },
	{ "s1",		R_S1,		0 },
	{ "s2",		R_S2,		0 },
	{ "s3",		R_S3,		0 },
	{ "s4",		R_S4,		0 },
	{ "s5",		R_S5,		0 },
	{ "s6",		R_S6,		0 },
	{ "s7",		R_S7,		0 },

	/* code generator registers */
	{ "r24",	R_R24,		0 },
	{ "r25",	R_R25,		0 },
	{ "t8",		R_T8,		0 },
	{ "t9",		R_T9,		0 },

	/* kernel temps registers */
	{ "r26",	R_R26,		0 },
	{ "r27",	R_R27,		0 },
	{ "k0",		R_K0,		0 },
	{ "k1",		R_K1,		0 },

	/* C linkage registers */
	{ "r28",	R_R28,		0 },
	{ "r29",	R_R29,		0 },
	{ "r30",	R_R30,		0 },
	{ "r31",	R_R31,		0 },
	{ "gp",		R_GP,		0 },
	{ "sp",		R_SP,		0 },
	{ "fp",		R_FP,		0 },
	{ "ra",		R_RA,		0 },

	/* IS THIS NECESSARY ???? */
	/* floating point registers */
	{ "f0",		R_F0,		0 },
	{ "f1",		R_F1,		0 },
	{ "f2",		R_F2,		0 },
	{ "f3",		R_F3,		0 },
	{ "f4",		R_F4,		0 },
	{ "f5",		R_F5,		0 },
	{ "f6",		R_F6,		0 },
	{ "f7",		R_F7,		0 },
	{ "f8",		R_F8,		0 },
	{ "f9",		R_F9,		0 },
	{ "f10",	R_F10,		0 },
	{ "f11",	R_F11,		0 },
	{ "f12",	R_F12,		0 },
	{ "f13",	R_F13,		0 },
	{ "f14",	R_F14,		0 },
	{ "f15",	R_F15,		0 },
	{ "f16",	R_F16,		0 },
	{ "f17",	R_F17,		0 },
	{ "f18",	R_F18,		0 },
	{ "f19",	R_F19,		0 },
	{ "f20",	R_F20,		0 },
	{ "f21",	R_F21,		0 },
	{ "f22",	R_F22,		0 },
	{ "f23",	R_F23,		0 },
	{ "f24",	R_F24,		0 },
	{ "f25",	R_F25,		0 },
	{ "f26",	R_F26,		0 },
	{ "f27",	R_F27,		0 },
	{ "f28",	R_F28,		0 },
	{ "f29",	R_F29,		0 },
	{ "f30",	R_F30,		0 },
	{ "f31",	R_F31,		0 },

	/* special processor registers */
	{ "pc",		R_EPC,		0 },
	{ "epc",	R_EPC,		0 },
	{ "mdhi",	R_MDHI,		0 },
	{ "mdlo",	R_MDLO,		0 },
	{ "sr",		R_SR,		sr_desc },
	{ "sr_r6000",	R_SR,		sr_desc_r6000 },
	{ "cause",	R_CAUSE,	cause_desc },
	{ "tlbhi",	R_TLBHI,	tlbhi_desc },
	{ "entryhi",	R_TLBHI,	tlbhi_desc },
	{ "tlblo",	R_TLBLO,	tlblo_desc },
	{ "entrylo",	R_TLBLO,	tlblo_desc },
	{ "badvaddr",	R_BADVADDR,	0 },
	{ "pid",	R_PID,		0 },
	{ "error",	R_ERROR,	error_desc_r6000 },
	{ "index",	R_INX,		tlbinx_desc },
	{ "inx",	R_INX,		tlbinx_desc },
	{ "context",	R_CTXT,		tlbctxt_desc },
	{ "ctxt",	R_CTXT,		tlbctxt_desc },
	{ "random",	R_RAND,		tlbrand_desc },
	{ "rand",	R_RAND,		tlbrand_desc },
	{ 0,		0,		0 }
};

 /* 
  * this struct is indexed according to machine_type so as to get the 
  * appropriate num of tlb entries 
  */
int _ntlbentries[] = {
			NTLBENTRIES,        /* M500 */
			NTLBENTRIES,        /* M800 */
			NTLBENTRIES,        /* M1000 */
			NTLBENTRIES,        /* M120 */
			NTLBENTRIES,        /* M2000 */
			NTLBENTRIES_6000,   /* M6000 */
                        NTLBENTRIES,        /* M20 */
			NTLBENTRIES,        /* M20 sable */
			NTLBENTRIES,        /* M180 */
			NTLBENTRIES,        /* Pizazz */
			NTLBENTRIES,        /* Genesis */
			};


static struct reg_table *lookup_reg();

/*
 * _get -- read memory or register
 */
_get(argc, argv)
int argc;
char **argv;
{
	unsigned address;
	unsigned val;
	struct reg_table *rt;
	int width = SW_WORD;
	extern char *atob();

	argv++; argc--;
	if (argc == 2 && **argv == '-') {
		switch ((*argv)[1]) {
		case 'b':
			width = SW_BYTE;
			break;

		case 'h':
			width = SW_HALFWORD;
			break;

		case 'w':
			width = SW_WORD;
			break;

		default:
			return(1);
		}
		argv++; argc--;
	}

	if (argc != 1)
		return(1);

	if (rt = lookup_reg(*argv)) {
		val = _get_register((int)rt->rt_index);
		if (rt->rt_desc)
			printf("%s:\t%d\t%R\n", *argv, val, val,
			       rt->rt_desc);
		else {
			printf("%s:\t%d\t0x%x\t'", *argv, val, val);
			showchar(val);
			puts("'\n");
		}
	} else {
		/* must be an address */
		if (*atob(*argv, &address))
			_dbg_error("Illegal address: %s", *argv);
			/* doesn't return */

		val = _get_memory(address, width);
		printf("0x%x:\t%d\t0x%x\t'", address, val, val);
		showchar(val);
		puts("'\n");
	}
	return(0);
}

/*
 * put -- write memory or register
 */
_put(argc, argv)
int argc;
char **argv;
{
	unsigned address;
	unsigned val;
	struct reg_table *rt;
	int width = SW_WORD;
	extern char *atob();

	argv++; argc--;
	if (argc == 3 && **argv == '-') {
		switch ((*argv)[1]) {
		case 'b':
			width = SW_BYTE;
			break;

		case 'h':
			width = SW_HALFWORD;
			break;

		case 'w':
			width = SW_WORD;
			break;

		default:
			return(1);
		}
		argv++; argc--;
	}

	if (argc != 2)
		return(1);

	if (*atob(argv[1], &val))
		_dbg_error("Illegal value: %s", argv[1]);
		/* doesn't return */

	if (rt = lookup_reg(*argv))
		_set_register((int)rt->rt_index, val);
	else {
		/* must be an address */
		if (*atob(*argv, &address))
			_dbg_error("Illegal address: %s", *argv);
			/* doesn't return */

		_set_memory(address, width, val);
	}
	return(0);
}

/*
 * lookup_reg -- returns reg_table entry
 */
static struct reg_table *
lookup_reg(name)
char *name;
{
	register struct reg_table *rt;

	for (rt = reg_table; rt->rt_name; rt++)
		if (streq(rt->rt_name, name)) {
			if (IS_R6300  &&  streq("sr",name))
				return(lookup_reg("sr_r6000"));
			return(rt);
		}
	return((struct reg_table *)0);
}

/*
 * string -- print memory as string
 */
_string(argc, argv)
char **argv;
{
	int maxlen = DEFAULT_STRLEN;
	register int c;
	char *addr;
	extern char *atob();

	if (argc <= 1 || argc > 3)
		return(1);

	if (*atob(argv[1], &addr))
		_dbg_error("illegal address: %s", argv[1]);

	if (argc == 3)
		if (*atob(argv[2], &maxlen))
			_dbg_error("illegal maxlen: %s", argv[2]);

	putchar('"');
	while ((c = _get_memory(addr++, SW_BYTE)) && maxlen-- > 0)
		showchar(c);
	puts("\"\n");
	return(0);
}

/*
 * call -- invoke client routine with arguments
 */
_call(argc, argv)
char **argv;
{
	int args[8];
	int i;
	int (*callp)();
	extern char *atob();

	if (argc < 2)
		return(1);

	if (argc > 8)
		_dbg_error("no more than 8 args can be passed with call");

	for (i = 0; i < argc - 2; i++)
		if (*atob(argv[i+2], &args[i]))
			_dbg_error("argument not numeric: %s", argv[i+2]);

	if (*atob(argv[1], &callp) || (unsigned)callp & 0x3)
		_dbg_error("bad routine address: %s", argv[1]);
	
	i = invoke(callp, args[0], args[1], args[2], args[3], args[4], args[5],
	    args[6], args[7]);

	printf("Routine returns %d 0x%x\n", i, i);
	return(0);
}

/* 
 * phystagdump RANGE -- dump the contents of the physical tags
 * in the given range. Only for Excalibur (6000)
 */
_phystagdump(argc,argv)
int argc;
char **argv;
{
	int addr, range_type, count;
	extern struct reg_desc ptag_desc_6000[];

	if(!IS_R6300){
		printf("Command not supported on this machine\n");
		return(0);
	}
	if (argc > 2)
		return(1);
	if (argc == 1) {
		printf("Bad Physical Tag spec: Need range \n");
		return(0);
	}
	else { /* correct arg number */
		range_type = parse_range(argv[1], &addr, &count);
		if (range_type == ERROR_RANGE)
			return(1);
		if (range_type == ADDR_RANGE)
			count = count - addr;
		if (count == 0)
			count = 1;
		if (count < 0 || addr < 0 || addr + count > NPTAGENTRIES) {
			printf("Bad Physical Tag spec: %s\n", argv[1]);
			return(0);
		}
	}
		
	while (count--) {
	   printf("\n%d:  Set 0 -  %R.  Set 1 -  %R.\n",addr,                              get_ptag(addr,0), ptag_desc_6000, get_ptag(addr,1),ptag_desc_6000);
	   addr++;
	}
	printf("\n");
	return(0);
}

/*
 * tlbdump [RANGE] -- dump contents of tlb
 * range is required for the 6000 . For the non-6000
 * dumps entire tlb if no range specified
 */
_tlbdump(argc, argv)
int argc;
char **argv;
{
	int addr, range_type, count;
	extern struct reg_desc tlblo_desc[], tlbhi_desc[], tlb_desc_6000[];
	

	if (argc > 2)
		return(1);
	if (argc == 1) {
		if(IS_R6300) {
		   printf("bad tlb spec: Need index with or without a range\n");
		   return(0);
		}
		else {
		   addr = 0;
		   count = MACHDEP(_ntlbentries);
		}
	} else {
		range_type = parse_range(argv[1], &addr, &count);
		if (range_type == ERROR_RANGE)
			return(1);
		if (range_type == ADDR_RANGE)
			count = count - addr;
		if (count == 0)
			count = 1;
		if (count < 0 || addr < 0 || 
		addr + count > MACHDEP(_ntlbentries)) {
			printf("bad tlb spec: %s\n", argv[1]);
			return(0);
		}
	}

	/* 
	 * can't read the STAGS for 6000s , 
	 * so only the tlb entries are printed for both side 0 and 1
	 */

	if(IS_R6300) {
		while (count--) {
		   printf("\n%d:  Set 0 -  %R.  Set 1 -  %R.\n",addr,                              get_tlb_6000(addr,0), tlb_desc_6000, get_tlb_6000(addr,1),                      tlb_desc_6000);
		   addr++;
		}
	}
	else {
		while (count--) {
		   printf("\n%d:\t%R\t%R\n", addr, get_tlblo(addr), tlblo_desc,
		    get_tlbhi(addr), tlbhi_desc);
		   addr++;
		}
	}
	printf("\n");
	return(0);
}

/*
 * tlbflush [RANGE] -- flush tlb translations
 * flushes no wired tlb entries if no RANGE given
 */
_tlbflush(argc, argv)
int argc;
char **argv;
{
	int addr, range_type, count;
	extern struct reg_desc tlblo_desc[], tlbhi_desc[];

	if(IS_R6300){
		printf("Command not supported on this machine\n");
		return(0);
	}
	if (argc > 2)
		return(1);
	if (argc == 1) {
		addr = TLBRANDOMBASE;
		count = MACHDEP(_ntlbentries);
	} else {
		range_type = parse_range(argv[1], &addr, &count);
		if (range_type == ERROR_RANGE)
			return(1);
		if (range_type == ADDR_RANGE)
			count = count - addr;
		if (count == 0)
			count = 1;
		if (count < 0 || addr < 0 || 
		addr + count > MACHDEP(_ntlbentries)) {
			printf("bad tlb spec: %s\n", argv[1]);
			return(0);
		}
	}
	while (count--)
		invaltlb(addr++);
	return(0);
}

/*
 * tlbvtop ADDRESS [PID] -- show physical address mapped to virtual ADDRESS
 * uses current pid if no PID specified
 */
_tlbvtop(argc, argv)
char **argv;
{
	register struct restart_blk *rb = (struct restart_blk *)RESTART_ADDR;
	unsigned pid, address, paddress;
	int index, side;

	if (argc < 2 || argc > 3)
		return(1);
	if (*atob(argv[1], &address)) {
		printf("bad address: %s\n", argv[1]);
		return(0);
	}
	if (argc == 3) {
		if (*atob(argv[2], &pid)) {
			printf("bad pid: %s\n", argv[2]);
			return(0);
		}
	} else
		pid = get_tlbpid();

	if (IS_R6300) {
		side = probe_tlb(address, pid);
		if(side < 0)
			printf("0x%x:%d not mapped by tlb\n", address, pid);
		else {
			index = (address >> VPNSHIFT_R6000) & 0x7ff; 
			printf("\nSet %d - index :%d\t%R\n\n",side,index,                               get_tlb_6000(index,side), tlb_desc_6000);
		}
	}
	else {
		index = probe_tlb(address, pid);
		if (index < 0)
			printf("0x%x:%d not mapped by tlb\n", address, pid);
		else
			printf("%d:\t%R\t%R\n", index, get_tlblo(index), 
			tlblo_desc, get_tlbhi(index), tlbhi_desc);
	}

	/*
	 * restart block vtop entry can only handle current pid!
	 */
	if (argc == 2 || pid == get_tlbpid()) {
		if (rb->rb_magic == RESTART_MAGIC && rb->rb_vtop) {
			paddress = (*rb->rb_vtop)(address);
			if (paddress != -1)
				printf("no vtop() mapping\n");
			else
				printf("vtop(0x%x) = 0x%x\n",
				    address, paddress);
		}
	}
	return(0);
}


/*
 * tlbunmap VADDR [PID] -- unmap the vitual address given if 
 * it is mapped.
 * uses current pid if no PID specified
 */
_tlbunmap(argc, argv)
char **argv;
{
	unsigned pid, address;
	int index, side;
	unsigned tlb_proto = 0;
	unsigned tlbhi_proto;


	if(!IS_R6300){
		printf("Command not supported on this machine\n");
		return(0);
	}
	if (argc < 2 || argc > 3)
		return(1);
	if (*atob(argv[1], &address)) {
		printf("bad address: %s\n", argv[1]);
		return(0);
	}
	if (argc == 3) {
		if (*atob(argv[2], &pid)) {
			printf("bad pid: %s\n", argv[2]);
			return(0);
		}
	} else
		pid = get_tlbpid();

	side = probe_tlb(address, pid);
	if(side < 0)
		printf("0x%x:%d not mapped by tlb\n", address, pid);
	else {
		index = (address >> VPNSHIFT_R6000) & 0x7ff; 
		tlbhi_proto = (address&VPNMASK_R6000) | (pid);
		tlbwired(side,tlbhi_proto,tlb_proto);
	}
	return(0);
}

/*
 * tlbptov PHYSADDR -- displays tlb entries mapping PHYSADDR
 */
_tlbptov(argc, argv)
char **argv;
{
	register index;
	unsigned address;
	int found = 0;

	if(IS_R6300){
		printf("Command not supported on this machine\n");
		return(0);
	}
	if (argc != 2)
		return(1);
	if (*atob(argv[1], &address)) {
		printf("bad address: %s\n", argv[1]);
		return(0);
	}
	address &= TLBLO_PFNMASK;
	for (index = 0; index < MACHDEP(_ntlbentries); index++)
		if ((get_tlblo(index)&TLBLO_PFNMASK) == address) {
			found++;
			printf("%d:\t%R\t%R\n", index, get_tlblo(index),
			    tlblo_desc, get_tlbhi(index), tlbhi_desc);
		}
	if (!found)
		printf("No mapping found to physical address 0x%x\n", address);
	return(0);
}

/*
 * tlbpid [PID] -- get or set dbgmon tlb pid
 */
_tlbpid(argc, argv)
char **argv;
{
	unsigned pid;
	unsigned pid_mask;

	if (argc > 2)
		return(1);
	if (argc == 1) {
		printf("Current dbgmon pid = %d\n", get_tlbpid());
		return(0);
	}
	if (IS_R6300)
		pid_mask = PIDMASK_R6000;
	else
		pid_mask = TLBHI_PIDMASK >> TLBHI_PIDSHIFT;
	if (*atob(argv[1], &pid)
	    || (pid & pid_mask) != pid) {
		printf("bad pid: %s\n", argv[1]);
		return(0);
	}
	set_tlbpid(pid);
	return(0);
}

/*
 * tlbmap [-i INDEX] [-(n|d|v|g)*] VADDR PADDR [PID]
 * maps VADDR to PADDR for the given PID (uses dbgmon pid if not specified)
 */
_tlbmap(argc, argv)
int argc;
char **argv;
{
	int index = TLBRANDOMBASE;
	int tmpinx;
	int pid;
	unsigned vaddr, paddr;
	unsigned tlblo_proto = 0;
	unsigned tlbhi_proto;
	char *cp;

	argc--; argv++;
	while (argc > 2 && **argv == '-') {
		cp = &(*argv)[1];
		do {
			switch (*cp) {
			case 'i':
				if (--argc < 1 || *atob(*++argv, &index))
					return(1);
				break;
				
			case 'n':
				tlblo_proto |= TLBLO_N;
				break;
				
			case 'v':
				tlblo_proto |= TLBLO_V;
				break;
				
			case 'd':
				tlblo_proto |= TLBLO_D;
				break;
				
			case 'g':
				tlblo_proto |= TLBLO_G;
				break;
			
			default:
				return(1);
			}
		} while (*++cp);
		argc--; argv++;
	}
	if (argc < 2)
		return(1);
	if (*atob(*argv, &vaddr)) {
		printf("bad vaddr: %s\n", *argv);
		return(1);
	}
	argv++;
	argc--;
	if (*atob(*argv, &paddr)) {
		printf("bad paddr: %s\n", *argv);
		return(1);
	}

	if(--argc){
		if (*atob(*argv, &pid)) {
			printf("bad pid: %s\n", *argv);
			return(1);
		}
	}
	else 
		pid = get_tlbpid();

	tmpinx = probe_tlb(vaddr, pid);
	if (tmpinx >= 0) {
		printf("0x%x:%d already mapped at index %d\n", vaddr,
		    pid, tmpinx);
		printf("%d:\t%R\t%R\n", tmpinx, get_tlblo(tmpinx),
		    tlblo_desc, get_tlbhi(tmpinx), tlbhi_desc);
		return(0);
	}
	tlbhi_proto = (vaddr&TLBHI_VPNMASK) | (pid <<TLBHI_PIDSHIFT);
	tlblo_proto |= (paddr&TLBLO_PFNMASK);
	tlbwired(index, tlbhi_proto, tlblo_proto);
	printf("%d:\t%R\t%R\n", index, get_tlblo(index),
	    tlblo_desc, get_tlbhi(index), tlbhi_desc);
	return(0);
}

/*
 * tlbmap -i SIDE [-(n|d|g)*] VADDR PADDR [PID]
 * maps VADDR to PADDR for the given PID (uses dbgmon pid if not specified)
 * for an M6000 -i SIDE indicates which side of tlb to map it on. Notice that
 * tlbmap will compulsorily set the valid bit for the tlb entry.
 */
_tlbmap_6000(argc, argv)
int argc;
char **argv;
{
	int side, index;
	int pid;
	int tmpside;
	unsigned vaddr, paddr;
	unsigned tlb_proto = 0;
	unsigned tlbhi_proto;
	char *cp;

	argc--; argv++;
	while (argc > 2 && **argv == '-') {
		cp = &(*argv)[1];
		do {
			switch (*cp) {
			case 'i':
				if (--argc < 1 || *atob(*++argv, &side))
					return(1);
				break;
				
			case 'n':
				tlb_proto |= TLB_N_R6000;
				break;
			case 'd':
				tlb_proto |= TLB_D_R6000;
				break;
				
			case 'g':
				tlb_proto |= TLB_G_R6000;
				break;
			
			default:
				return(1);
			}
		} while (*++cp);
		argc--; argv++;
	}
	if (argc < 2)
		return(1);
	if (*atob(*argv, &vaddr)) {
		printf("bad vaddr: %s\n", *argv);
		return(1);
	}
	argv++;
	argc--;
	if (*atob(*argv, &paddr)) {
		printf("bad paddr: %s\n", *argv);
		return(1);
	}
	if(--argc){
		if (*atob(*argv, &pid)) {
			printf("bad pid: %s\n", *argv);
			return(1);
		}
	}
	else 
		pid = get_tlbpid();

	tmpside = probe_tlb(vaddr, pid);
	if (tmpside >= 0) {
	     index = (vaddr >> VPNSHIFT_R6000) & 0x7ff;
	     printf("\n0x%x for pid:%d already mapped in set %d at index %d\n", 
	     vaddr,pid, tmpside, index);
	     printf("\nSet %d -\tindex  :%d\t%R\n\n",tmpside,index,                           get_tlb_6000(index,tmpside), tlb_desc_6000);
	     return(0);
	}
	tlbhi_proto = (vaddr&VPNMASK_R6000) | (pid);
	tlb_proto |= ((paddr&TLB_PFNMASK_R6000) | (TLB_V_R6000));
	tlbwired(side, tlbhi_proto, tlb_proto);
	index = (vaddr >> VPNSHIFT_R6000) & 0x7ff;
	printf("\nSet %d -\tindex :%d\t%R\n\n",side,index,                              get_tlb_6000(index,side),tlb_desc_6000);
	return(0);
}

_cacheflush(argc, argv)
char **argv;
{
	int addr, count, range_type;

	if (argc > 2)
		return(1);
	if (argc == 1) {
		flush_cache();
	} else {
		range_type = parse_range(argv[1], &addr, &count);
		if (range_type == ERROR_RANGE)
			return(1);
		if (range_type == ADDR_RANGE)
			count = count - addr;
		if (count == 0)
			count = 1;
		if (count < 0) {
			printf("bad cache spec: %s\n", argv[1]);
			return(0);
		}
		clear_cache(addr, count);
	}
	return(0);
}

/* @(#)getenv.c	4.1 (Berkeley) 12/21/80 */
/*
 *	getenv(name)
 *	returns ptr to value associated with name, if any, else NULL
 */
extern char **environ;
static char *nvmatch();

char *
getenv(name)
char *name;
{
	register char **p = environ;
	register char *v;

	if (p)
		while (*p != NULL)
			if ((v = nvmatch(name, *p++)) != NULL)
				return(v);
	return(NULL);
}

/*
 *	s1 is either name, or name=value
 *	s2 is name=value
 *	if names match, return value of s2, else NULL
 *	used for environment searching: see getenv
 */

static char *
nvmatch(s1, s2)
register char *s1, *s2;
{

	while (*s1 == *s2++)
		if (*s1++ == '=')
			return(s2);
	if (*s1 == '\0' && *(s2-1) == '=')
		return(s2);
	return(NULL);
}
