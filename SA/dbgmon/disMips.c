#ident "$Header: disMips.c,v 1.3 90/01/11 14:16:08 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * disMips.c -- MIPS Instruction Printer
 */

#include "dbgmon/dbgmon.h"
#include "dbgmon/inst.h"
#include "saio/setjmp.h"
#include "machine/cpu_board.h"

extern int machine_type;

/* register definitions */
#define ZERO 0
#define GP 28
#define FP 30

static char *op_name[64] = {
/* 0 */	"spec",	"bcond","j",	"jal",	"beq",	"bne",	"blez",	"bgtz",
/* 8 */	"addi",	"addiu","slti",	"sltiu","andi",	"ori",	"xori",	"lui",
/*16 */	"cop0",	"cop1",	"cop2",	"cop3",	"op50",	"op54",	"op58",	"op5c",
/*24 */	"op60",	"op64",	"op68",	"op6c",	"op70",	"op74",	"op78",	"op7c",
/*32 */	"lb",	"lh",	"lwl",	"lw",	"lbu",	"lhu",	"lwr",	"ld",
/*40 */	"sb",	"sh",	"swl",	"sw",	"opb0",	"opb4",	"swr",	"sd",
/*48 */	"lwc0",	"lwc1",	"lwc2",	"lwc3",	"ldc0",	"ldc1",	"ldc2",	"ldc3",
/*56 */	"swc0",	"swc1",	"swc2",	"swc3",	"sdc0",	"sdc1",	"sdc2",	"sdc3"
};

static char *spec_name[64] = {
/* 0 */	"sll",	"spec01","srl",	"sra",	"sllv",	"spec05","srlv","srav",
/* 8 */	"jr",	"jalr",	"spec12","spec13","syscall","break","vcall","spec17",
/*16 */	"mfhi",	"mthi",	"mflo",	"mtlo",	"spec24","spec25","spec26","spec27",
/*24 */	"mult",	"multu","div",	"divu",	"spec34","spec35","spec36","spec37",
/*32 */	"add",	"addu",	"sub",	"subu",	"and",	"or",	"xor",	"nor",
/*40 */	"spec50","spec51","slt","sltu",	"spec54","spec55","spec56","spec57",
/*48 */	"spec60","spec61","spec62","spec63","spec64","spec65","spec66","spec67",
/*56 */	"spec70","spec71","spec72","spec73","spec74","spec75","spec76","spec77"
};

static char *bcond_name[32] = {
	"bltz",
	"bgez"
};

static char *cop1_name[64] = {
/* 0 */	"fadd",	"fsub",	"fmpy",	"fdiv",	"fsqrt","fabs",	"fmov",	"fop07",
/* 8 */	"fop08","fop09","fop0a","fop0b","fop0c","fop0d","fop0e","fop0f",
/*16 */	"fop10","fop11","fop12","fop13","fop14","fop15","fop16","fop17",
/*24 */	"fop18","fop19","fop1a","fop1b","fop1c","fop1d","fop1e","fop1f",
/*32 */	"fcvts","fcvtd","fcvte","fop23","fcvtw","fop25","fop26","fop27",
/*40 */	"fop28","fop29","fop2a","fop2b","fop2c","fop2d","fop2e","fop2f",
/*48 */	"fcmp0","fcmp1","fcmp2","fcmp3","fcmp4","fcmp5","fcmp6","fcmp7",
/*56 */	"fcmp8","fcmp9","fcmpa","fcmpb","fcmpc","fcmpd","fcmpe","fcmpf"
};

static char *fmt_name[16] = {
	"s",	"d",	"e",	"fmt3",
	"w",	"fmt5",	"fmt6",	"fmt7",
	"fmt8",	"fmt9",	"fmta",	"fmtb",
	"fmtc",	"fmtd",	"fmte",	"fmtf"
};

static char *sbregister_name[2][32] = {
	{	/* compiler names */
		"zero",	"at",	"v0",	"v1",	"a0",	"a1",	"a2",	"a3",
		"t0",	"t1",	"t2",	"t3",	"t4",	"t5",	"t6",	"t7",
		"s0",	"s1",	"s2",	"s3",	"s4",	"s5",	"s6",	"s7",
		"t8",	"t9",	"k0",	"k1",	"gp",	"sp",	"fp",	"ra"
	},
	{	/* hardware names */
		"r0",	"r1",	"r2",	"r3",	"r4",	"r5",	"r6",	"r7",
		"r8",	"r9",	"r10",	"r11",	"r12",	"r13",	"r14",	"r15",
		"r16",	"r17",	"r18",	"r19",	"r20",	"r21",	"r22",	"r23",
		"r24",	"r25",	"r26",	"r27",	"gp",	"sp",	"fp",	"r31"
	}
};

static char *c0_opname[32] = {
	"c0op0","tlbr",	"tlbwi","c0op3","c0op4","c0op5","tlbwr","c0op7",
	"tlbp",	"c0op9","c0op10","c0op11","c0op12","c0op13","c0op14","c0op15",
	"rfe",	"c0op17","c0op18","c0op19","c0op20","c0op21","c0op22","c0op23",
	"c0op24","c0op25","c0op26","c0op27","c0op28","c0op29","c0op30","c0op31"
};

static char *c0_reg[32] = {
	"index","random","tlblo","c0r3","context","c0r5","c0r6","c0r7",
	"badvaddr","c0r9","tlbhi","c0r11","sr",	"cause","epc",	"prid",
	"c0r16","c0r17","c0r18","c0r19","c0r20","c0r21","c0r22","c0r23",
	"c0r24","c0r25","c0r26","c0r27","c0r28","c0r29","c0r30","c0r31"
};

static char *c0_reg_r6000[32] = {
	"c0r0","c0r1","c0r2","c0r3","c0r4","c0r5","pidmask","error",
	"badvaddr","c0r9","pid","c0r11","sr",	"cause","epc",	"prid",
	"c0r16","c0r17","c0r18","c0r19","c0r20","c0r21","c0r22","c0r23",
	"c0r24","c0r25","c0r26","c0r27","c0r28","c0r29","c0r30","c0r31"
};

static int regcount;		/* how many regs used in this inst */
static int regnum[6];		/* which regs used in this inst */

extern unsigned _regs[];

static char *
register_name (ireg, regstyle)
{
	int	i;

	for (i = 0; i < regcount; i++)
		if (regnum[i] == ireg)
			break;
	if (i >= regcount)
		regnum[regcount++] = ireg;
	return (sbregister_name[regstyle][ireg]);
}

_show_inst(epc)
unsigned epc;
{
	int regstyle;

	atob(getenv("regstyle"), &regstyle);
	_PrintInstruction(epc, regstyle, 1);
}

_PrintInstruction(iadr, regstyle, showregs)
unsigned iadr;
{
	union mips_instruction i;
	char *s;
	int print_next;
	int ireg;
	int ibytes;
	short simmediate;
	jmp_buf pi_buf;
	extern int *nofault;

	regcount = 0;
	print_next = 0;
	ibytes = 4;
	regstyle = (regstyle) ? 1 : 0;	/* make sure its valid */

	printf("0x%x:\t", iadr);

	if (setjmp(pi_buf)) {
		sa_spl();
		printf("unaccessable\n");
		return(ibytes);
	}
	nofault = pi_buf;
	i.word = _get_memory(iadr, SW_WORD);
	nofault = 0;
	if (!showregs) {
		printhex(i.word);
		putchar('\t');
	}

	switch (i.j_format.opcode) {
	case spec_op:
		if (i.word == 0) {
			printf("nop");
			break;
		}
		else if (i.r_format.func == addu_op && i.r_format.rt == ZERO) {
			printf("move\t%s,%s",
			    register_name(i.r_format.rd, regstyle),
			    register_name(i.r_format.rs, regstyle));
			break;
		}
		printf("%s", spec_name[i.r_format.func]);
		switch (i.r_format.func) {
		case sll_op:
		case srl_op:
		case sra_op:
			printf("\t%s,%s,%d",
			    register_name(i.r_format.rd, regstyle),
			    register_name(i.r_format.rt, regstyle),
			    i.r_format.re);
			break;
		case sllv_op:
		case srlv_op:
		case srav_op:
			printf("\t%s,%s,%s",
			    register_name(i.r_format.rd, regstyle),
			    register_name(i.r_format.rt, regstyle),
			    register_name(i.r_format.rs, regstyle));
			break;
		case mfhi_op:
		case mflo_op:
			printf("\t%s", register_name(i.r_format.rd, regstyle));
			break;
		case jr_op:
		case jalr_op:
			print_next = 1;
			/* fall through */
		case mtlo_op:
		case mthi_op:
			printf("\t%s", register_name(i.r_format.rs, regstyle));
			break;
		case mult_op:
		case multu_op:
		case div_op:
		case divu_op:
			printf("\t%s,%s",
			    register_name(i.r_format.rs, regstyle),
			    register_name(i.r_format.rt, regstyle));
			break;
		case syscall_op:
			break;
		case break_op:
		case vcall_op:
			printf("\t%d", i.r_format.rs*32+i.r_format.rt);
			break;
		default:
			printf("\t%s,%s,%s",
			    register_name(i.r_format.rd, regstyle),
			    register_name(i.r_format.rs, regstyle),
			    register_name(i.r_format.rt, regstyle));
			break;
		};
		break;
	case bcond_op:
		printf("%s\t%s,",
		    bcond_name[i.i_format.rt],
		    register_name(i.i_format.rs, regstyle));
		goto branch_displacement;
	case blez_op:
	case bgtz_op:
		printf("%s\t%s,", op_name[i.i_format.opcode],
		    register_name(i.i_format.rs, regstyle));
		goto branch_displacement;
	case beq_op:
		if (i.i_format.rs == ZERO && i.i_format.rt == ZERO) {
			printf("b\t");
			goto branch_displacement;
		}
		/* fall through */
	case bne_op:
		printf("%s\t%s,%s,", op_name[i.i_format.opcode],
		    register_name(i.i_format.rs, regstyle),
		    register_name(i.i_format.rt, regstyle));
branch_displacement:
		simmediate = i.i_format.simmediate;
		printf("0x%x", iadr+4+(simmediate<<2));
		print_next = 1;
		break;
	case cop0_op:
		switch (i.r_format.rs) {
		case bc_op:
			printf("bc0%c\t", "ft"[i.r_format.rt]);
			goto branch_displacement;
		case mtc_op:
			printf("mtc0\t%s,%s",
			    register_name(i.r_format.rt, regstyle),
			    ((IS_R6300) ? c0_reg_r6000[i.f_format.rd]
					: c0_reg[i.f_format.rd]));
			break;
		case mfc_op:
			printf("mfc0\t%s,%s",
			    register_name(i.r_format.rt, regstyle),
			    ((IS_R6300) ? c0_reg_r6000[i.f_format.rd]
					: c0_reg[i.f_format.rd]));
			break;
		default:
			printf("c0\t%s", c0_opname[i.f_format.func]);
			break;
		};
		break;
	case cop1_op:
		switch (i.r_format.rs) {
		case bc_op:
			printf("bc1%c\t", "ft"[i.r_format.rt]);
			goto branch_displacement;
		case mtc_op:
			printf("mtc1\t%s,f%d",
			    register_name(i.r_format.rt, regstyle),
			    i.f_format.rd);
			break;
		case mfc_op:
			printf("mfc1\t%s,f%d",
			    register_name(i.r_format.rt, regstyle),
			    i.f_format.rd);
			break;
		default:
			printf("%s.%s\tf%d,f%d,f%d",
			    cop1_name[i.f_format.func],
			    fmt_name[i.f_format.fmt],
			    i.f_format.re, i.f_format.rd, i.f_format.rt);
			break;
		};
		break;

	case j_op:
	case jal_op:
		printf("%s\t", op_name[i.j_format.opcode]);
		printf("0x%x", ((iadr+4)&~((1<<28)-1)) +(i.j_format.target<<2));
		print_next = 1;
		break;

	case swc1_op:
	case sdc1_op:
	case lwc1_op:
	case ldc1_op:
		printf("%s\tf%d,", op_name[i.i_format.opcode],
		    i.i_format.rt);
		goto loadstore;

	case lb_op:
	case lh_op:
	case lw_op:
	case ld_op:
	case lbu_op:
	case lhu_op:
	case sb_op:
	case sh_op:
	case sw_op:
	case sd_op:
		printf("%s\t%s,", op_name[i.i_format.opcode],
		    register_name(i.i_format.rt, regstyle));
loadstore:
		simmediate = i.i_format.simmediate;
		printf("%d(%s)", simmediate,
		    register_name(i.i_format.rs, regstyle));
		if (showregs) {
			simmediate = i.i_format.simmediate;
			printf(" <0x%x>", simmediate +
			    _regs[i.i_format.rs]);
		}
		break;

	case ori_op:
	case xori_op:
		if (i.u_format.rs == ZERO) {
			printf("li\t%s,%d",
			    register_name(i.u_format.rt, regstyle),
			    i.u_format.uimmediate);
			break;
		}
		/* fall through */
	case andi_op:
		printf("%s\t%s,%s,0x%x", op_name[i.u_format.opcode],
		    register_name(i.u_format.rt, regstyle),
		    register_name(i.u_format.rs, regstyle),
		    i.u_format.uimmediate);
		break;
	case lui_op:
		printf("%s\t%s,0x%x", op_name[i.u_format.opcode],
		    register_name(i.u_format.rt, regstyle),
		    i.u_format.uimmediate);
		break;
	case addi_op:
	case addiu_op:
		if (i.i_format.rs == ZERO) {
			simmediate = i.i_format.simmediate;
			printf("li\t%s,%d",
			    register_name(i.i_format.rt, regstyle),
			    simmediate);
			break;
		}
#if 0
		else if (i.i_format.simmediate == 0) {
			printf("move\t%s,%s",
			    register_name(i.i_format.rt, regstyle),
			    register_name(i.i_format.rs, regstyle));
			break;
		}
#endif
		/* fall through */
	default:
		simmediate = i.i_format.simmediate;
		printf("%s\t%s,%s,%d", op_name[i.i_format.opcode],
		    register_name(i.i_format.rt, regstyle),
		    register_name(i.i_format.rs, regstyle),
		    simmediate);
		break;
	}

	/* print out the registers use in this inst */
	if (showregs && regcount > 0) {
		puts("\t<");
		for (ireg = 0; ireg < regcount; ireg++) {
			if (ireg != 0)
				putchar(',');
			printf("%s=0x%x",
			    sbregister_name[regstyle][regnum[ireg]],
			    _regs[regnum[ireg]]);
		}
		putchar('>');
	}
	putchar('\n');
	if (print_next) {
		_PrintInstruction(iadr+4, regstyle, showregs);
		ibytes += 4;
	}
	return(ibytes);
}

static char hexdigits[] = "0123456789abcdef";

static
printhex(word)
register unsigned int word;
{
	register int i;

	for (i = 28; i >= 0; i -= 4)
		putchar(hexdigits[(word >> i) & 0xf]);
}
