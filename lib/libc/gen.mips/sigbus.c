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
#ident	"$Header: sigbus.c,v 1.3.2.2 90/05/10 01:26:40 wje Exp $"

#include "sys/sbd.h"
#include "sys/fpu.h"
#include "sys/inst.h"
#include "signal.h"
#include "stdio.h"

#define BYTE_ALIGNED_LH 0
#define BYTE_ALIGNED_SH 1
#define HALF_ALIGNED_LW 2
#define HALF_ALIGNED_SW 3
#define BYTE_ALIGNED_LW 4
#define BYTE_ALIGNED_SW 5
#define HALF_ALIGNED_LWC1 6
#define HALF_ALIGNED_SWC1 7
#define BYTE_ALIGNED_LWC1 8
#define BYTE_ALIGNED_SWC1 9
#define NUNALIGNED 10
static unsigned sigbus_counts[NUNALIGNED];

static char *sigbus_count_names[NUNALIGNED] = {
	"byte aligned lh",
	"byte aligned sh",
	"half aligned lw",
	"half aligned sw",
	"byte aligned lw",
	"byte aligned sw",
	"half aligned lwc1",
	"half aligned swc1",
	"byte aligned lwc1",
	"byte aligned swc1"
};

#define PCTSIZE 8192
#define PCTMASK (PCTSIZE-1)
static struct {
    unsigned pc, count;
} pctable[PCTSIZE];
static pctable_entries;

static void
sigbus (sig, code, scp)
    register int sig, code;
    register struct sigcontext *scp;
{
    union mips_instruction cpu_instr;
    unsigned long branch_instr;
    register long addr;
    register unsigned h;

        h = scp->sc_pc & PCTMASK;
        while (1) {
	    if (pctable[h].pc == scp->sc_pc || pctable[h].pc == 0) break;
	    h = (h + 1) & PCTMASK;
	}
	if (pctable[h].pc != 0 || ++pctable_entries <= PCTSIZE*3/4) {
	    pctable[h].pc = scp->sc_pc;
	    pctable[h].count += 1;
	}

	/*
	 * If the instruction was in a branch delay slot
	 * then emulate the branch to determine the target pc to return to.
	 * Else the target pc to return to is just address of the next
	 * instruction.
	 */
	if (scp->sc_cause & CAUSE_BD){
	    branch_instr = *(unsigned long *)(scp->sc_pc);
	    cpu_instr.word = *(unsigned long *)(scp->sc_pc + 4);
	    if (emulate_branch(scp, branch_instr) != 0){
		fprintf(stderr, "sigbus(): Error: Can't emulate branch instruction: 0x%x at: 0x%x\n", branch_instr, scp->sc_pc);
		exit(1);
	    }
	}
	else{
	    cpu_instr.word = *(unsigned long *)(scp->sc_pc);
	    scp->sc_pc += sizeof(unsigned long);
	}

	addr = scp->sc_regs[cpu_instr.i_format.rs] +
	       cpu_instr.i_format.simmediate;

	switch (cpu_instr.r_format.opcode){
	case lw_op:
	    if ((addr & 1) != 0) {
		sigbus_counts[BYTE_ALIGNED_LW] += 1;
	    }
	    else {
		sigbus_counts[HALF_ALIGNED_LW] += 1;
	    }
	    scp->sc_regs[cpu_instr.i_format.rt] = unaligned_load_word(addr);
	    break;
	case lwc1_op:
	    if ((addr & 1) != 0) {
		sigbus_counts[BYTE_ALIGNED_LWC1] += 1;
	    }
	    else {
		sigbus_counts[HALF_ALIGNED_LWC1] += 1;
	    }
	    scp->sc_fpregs[cpu_instr.i_format.rt] = unaligned_load_word(addr);
	    break;
	case lh_op:
	    sigbus_counts[BYTE_ALIGNED_LH] += 1;
	    scp->sc_regs[cpu_instr.i_format.rt] = unaligned_load_half(addr);
	    break;
	case lhu_op:
	    sigbus_counts[BYTE_ALIGNED_LH] += 1;
	    scp->sc_regs[cpu_instr.i_format.rt] = unaligned_load_uhalf(addr);
	    break;

	case sw_op:
	    if ((addr & 1) != 0) {
		sigbus_counts[BYTE_ALIGNED_SW] += 1;
	    }
	    else {
		sigbus_counts[HALF_ALIGNED_SW] += 1;
	    }
	    unaligned_store_word(addr, scp->sc_regs[cpu_instr.i_format.rt]);
	    break;
	case swc1_op:
	    if ((addr & 1) != 0) {
		sigbus_counts[BYTE_ALIGNED_SWC1] += 1;
	    }
	    else {
		sigbus_counts[HALF_ALIGNED_SWC1] += 1;
	    }
	    unaligned_store_word(addr, scp->sc_fpregs[cpu_instr.i_format.rt]);
	    break;
	case sh_op:
	    sigbus_counts[BYTE_ALIGNED_SH] += 1;
	    unaligned_store_half(addr, scp->sc_regs[cpu_instr.i_format.rt]);
	    break;
	default:
	    fprintf(stderr, "sigbus(): non-load/store instruction. Exiting\n");
	    exit(1);
	}

	sigreturn(scp);
}

void print_unaligned_summary();

void
print_unaligned_summary_()
{
	print_unaligned_summary();
}
void
print_unaligned_summary()
{
    register unsigned i, n, t, c;
    register int j;
    fprintf (stderr, " ##############################################\n");
    fprintf (stderr, " #         unaligned reference summary        #\n");
    t = 0;
    for (i = 0; i != NUNALIGNED; i += 1) t += sigbus_counts[i];
    for (i = 0; i != NUNALIGNED; i += 1) {
	if (sigbus_counts[i] != 0) {
	    fprintf (stderr, " # %-17s %10u %5.1f%%        #\n",
		     sigbus_count_names[i],
		     sigbus_counts[i],
		     (double)sigbus_counts[i]/(double)t*100.0);
	}
    }
    /* insertion sort pc table */
    n = 0;
    t = 0;
    for (i = 0; i != PCTSIZE; i += 1) {
	register unsigned pc = pctable[i].pc;
	register unsigned count = pctable[i].count;
	if (pc != 0) {
	    for (j = n-1; j >= 0 && pctable[j].count < count; j -= 1) {
		pctable[j+1].pc = pctable[j].pc;
		pctable[j+1].count = pctable[j].count;
	    }
	    pctable[j+1].pc = pc;
	    pctable[j+1].count = count;
	    n += 1;
	    t += count;
	}
    }
    c = 0;
    for (i = 0; i != n; i += 1) {
	c += pctable[i].count;
	fprintf (stderr, " # 0x%.8x/i      %10u %5.1f%% %5.1f%% #\n",
		 pctable[i].pc,
		 pctable[i].count,
		 (double)pctable[i].count/(double)t*100.0,
		 (double)c/(double)t*100.0);
    }
    fprintf (stderr, " ##############################################\n");
}

void handle_unaligned_traps();

void
handle_unaligned_traps_()
{
	handle_unaligned_traps();
}
void
handle_unaligned_traps()
{
    sigset(SIGBUS, sigbus);
}
