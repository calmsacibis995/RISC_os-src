#ident "$Header: brkpt.c,v 1.4 90/09/17 18:11:35 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright 
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * brkpt.c -- support routines for dbgmon breakpoint and single step
 * facilities
 */

/*
 * TODO:
 *	Need to deal with dbg_error stuff in step1 when running
 *	debug protocol.
 */

#include "machine/cpu.h"
#include "saio/setjmp.h"
#include "saio/parser.h"
#include "saio/saioctl.h"
#include "dbgmon/dbgmon.h"
#include "dbgmon/inst.h"

#define	NBREAKPOINTS	16

int *bp_nofault;	/* nofault ptr to jmp_buf for rdebug usage with bp's */

static struct brkpt_table {
	unsigned bt_addr;		/* address where brkpt is installed */
	unsigned bt_inst;		/* previous instruction */
	int bt_type;			/* breakpoint type */
	int bt_oldtype;			/* type before being suspended */
} brkpt_table[NBREAKPOINTS];

unsigned int return_addr;		/* save for single stepping */

/*
 * Types of breakpoints
 *
 * NOTE: order of types is sensitive, see new_brkpt().
 *
 * Suspended breakpoints are breakpoints that are suspended for one
 * instruction to allow continuing past that breakpoint.  They
 * are replaced as soon as execution is past that instruction.
 *
 * Continue breakpoints are placed immediately after the instruction
 * where a suspended breakpoint was, they are placed there so that the
 * suspended breakpoint may be reinstated.
 *
 * Temporary breakpoints are used for "step" and "goto" commands
 * where the breakpoint should be removed as soon as it is taken.
 *
 * Permanent breakpoints are put down by the "brk" command.
 */
#define	BTTYPE_EMPTY	0		/* unused entry */
#define	BTTYPE_SUSPEND	1		/* suspended breakpoint */
#define	BTTYPE_CONT	2		/* continue breakpoint */
#define	BTTYPE_TEMP	3		/* temporary breakpoint */
#define	BTTYPE_PERM	4		/* permanent breakpoint */

extern int _regs[];
void _clear_brkpts();
static unsigned branch_target();
static void step1();
static void step2();
static void bp_continue();
static void new_brkpt();
static void suspend_brkpt();

/*
 * _brk -- set breakpoints
 */
_brk(argc, argv)
char **argv;
{
	register i;
	unsigned addr;
	extern char *atob();

	if (argc == 1) {
		for (i = 0; i < NBREAKPOINTS; i++)
			if (brkpt_table[i].bt_type != BTTYPE_EMPTY)
				printf("%d:\t0x%x\n",
				    i, brkpt_table[i].bt_addr);
		return(0);
	}
	while (--argc > 0) {
		argv++;
		if (*atob(*argv, &addr)) {
			printf("illegal address: %s\n", *argv);
			continue;
		}
		new_brkpt(addr, BTTYPE_PERM);
	}
	return(0);
}

static int step_count;
static void (*step_routine)();

/*
 * _go_to -- set temporary breakpoints and resume
 */
_go_to(argc, argv)
char **argv;
{
	register i;
	unsigned addr;
	extern char *atob();

	while (--argc > 0) {
		argv++;
		if (*atob(*argv, &addr)) {
			printf("illegal address: %s\n", *argv);
			continue;
		}
		new_brkpt(addr, BTTYPE_TEMP);
	}
	step_count = 1;
	bp_continue();
	/* doesn't return */
}

/*
 * _unbrk -- remove breakpoints
 */
_unbrk(argc, argv)
char **argv;
{
	register i;
	int bpnum;
	extern char *atob();

	if (argc == 1) {
		_clear_brkpts();
		return(0);
	}
	while (--argc > 0) {
		argv++;
		if (*atob(*argv, &bpnum) || bpnum<0 || bpnum>=NBREAKPOINTS
		    || brkpt_table[bpnum].bt_type == BTTYPE_EMPTY) {
			printf("illegal breakpoint number: %s\n", *argv);
			continue;
		}
		brkpt_table[bpnum].bt_type = BTTYPE_EMPTY;
	}
	return(0);
}

/*
 * _step -- single instruction execution
 */
_step(argc, argv)
char **argv;
{
	extern char *atob();

	if (argc > 2)
		return(1);

	step_count = 1;
	if (argc == 2)
		if (*atob(*++argv, &step_count) || step_count <= 0)
			_dbg_error("illegal count: %s\n", *argv);

	step1(BTTYPE_TEMP);
	/* doesn't return */
}

/*
 * step1 -- implementation routine for step
 */
static void
step1(bp_type)
{
	unsigned next_inst;
	unsigned branch_pc;

	next_inst = _get_memory(_regs[R_EPC], SW_WORD);
	if (is_branch(next_inst)) {
		/* save address for implementation of stepping over rom */
		if (is_jal(next_inst)) return_addr = _regs[R_EPC]+8;

		if (is_branch(_get_memory(_regs[R_EPC]+4, SW_WORD)))
			_dbg_error("branch in branch delay slot");
		branch_pc = branch_target(next_inst, _regs[R_EPC]);
		if (branch_pc == _regs[R_EPC]) {
			if (is_conditional(next_inst))
				printf("WARNING: stepping over self-branch\n");
			else
				_dbg_error("can't step over self-branch");
		} else {
			if (is_rom(branch_pc))
				new_brkpt(return_addr, BTTYPE_TEMP);
			else
				new_brkpt(branch_pc, bp_type);
		}
		if (is_conditional(next_inst))
			new_brkpt(_regs[R_EPC] + 8, bp_type);
	} else
		new_brkpt(_regs[R_EPC] + 4, bp_type);
	step_routine = step1;
	bp_continue();
}

single_step()
{
	step_count = 1;
	step1(BTTYPE_TEMP);
}

/*
 * _Step -- execute single instruction, "stepping over" subroutines
 */
_Step(argc, argv)
char **argv;
{
	extern char *atob();

	if (argc > 2)
		return(1);

	step_count = 1;
	if (argc == 2)
		if (*atob(*++argv, &step_count) || step_count <= 0)
			_dbg_error("illegal count: %s\n", *argv);

	step2(BTTYPE_TEMP);
}

/*
 * step2 -- implementation routine for Step
 */
static void
step2(bp_type)
{
	unsigned next_inst;
	unsigned branch_pc;

	next_inst = _get_memory(_regs[R_EPC], SW_WORD);
	if (is_branch(next_inst)) {
		if (is_branch(_get_memory(_regs[R_EPC]+4, SW_WORD)))
			_dbg_error("branch in branch delay slot");
		if (is_jal(next_inst))
			new_brkpt(_regs[R_EPC] + 8, bp_type);
		else {
			branch_pc = branch_target(next_inst, _regs[R_EPC]);
			if (branch_pc == _regs[R_EPC]) {
				if (is_conditional(next_inst))
				    printf("WARNING: stepping self-branch\n");
				else
				    _dbg_error("can't step over self-branch");
			} else
				new_brkpt(branch_pc, bp_type);
			if (is_conditional(next_inst))
				new_brkpt(_regs[R_EPC] + 8, bp_type);
		}
	} else
		new_brkpt(_regs[R_EPC] + 4, bp_type);
	step_routine = step2;
	bp_continue();
}

/*
 * _disass -- disassemble mips instructions
 */
_disass(argc, argv)
char **argv;
{
	int count;
	unsigned addr;
	int regstyle;
	int ibytes;
	int range_type;
	char *cp;
	extern char *getenv();
	extern char *atob();

	if (argc != 2)
		return(1);

	range_type = parse_range(argv[1], &addr, &count);
	if (range_type == ERROR_RANGE)
		return(1);
	if (range_type == ADDR_RANGE)
	{
	    if (addr > count)
		_dbg_error("illegal address range");
	    else			
		count = ((unsigned)count - addr + 1) / 4;
	}
	if (count == 0)
		count = 1;

	if (addr & 0x3)
		_dbg_error("address not word aligned: %s", argv[1]);

	regstyle = 0;
	cp = getenv("regstyle");
	if (cp)
		atob(cp, &regstyle);

	while (count > 0) {
		ibytes = _PrintInstruction(addr, regstyle, 0);
		addr += ibytes;
		count -= ibytes >> 2;
	}

	return(0);
}

/*
 * cont -- continue command, transfer control to client code
 */
_cont(argc, argv)
int argc;
char **argv;
{
	if (argc != 1)
		return(1);

	bp_continue();
}

/*
 * bp_continue -- resume execution dealing with breakpoints
 */
static void
bp_continue()
{
	int bp1, bp2;
	unsigned pc;
	unsigned inst;

	pc = _regs[R_EPC];
	inst = _get_memory(pc, SW_WORD);
	bp1 = is_brkpt(pc);
	bp2 = is_branch(inst) && is_brkpt(pc + 4);
	if (bp1 || bp2) {
		if (bp1)
			suspend_brkpt(pc);
		if (bp2)
			suspend_brkpt(pc + 4);
		step1(BTTYPE_CONT);
		/* doesn't return */
	}
	_resume();
}

/*
 * new_brkpt -- add new breakpoint to breakpoint table
 */
static void
new_brkpt(addr, type)
unsigned addr;
{
	register struct brkpt_table *bt, *ebt;

	if (addr & 0x3)
		_dbg_error("not on instruction boundry");

	ebt = NULL;
	for (bt = brkpt_table; bt < &brkpt_table[NBREAKPOINTS]; bt++) {
		if (bt->bt_type == BTTYPE_EMPTY && ebt == NULL)
			ebt = bt;
		if (bt->bt_type != BTTYPE_EMPTY && addr == bt->bt_addr) {
			if (bt->bt_type == BTTYPE_SUSPEND)
				_fatal_error("new_brkpt");
			if (type > bt->bt_type)
				bt->bt_type = type;
			return;
		}
	}
	if (ebt == NULL)
		_dbg_error("breakpoint table full");
	ebt->bt_addr = addr;
	ebt->bt_type = type;
}

/*
 * install_brkpts -- install breakpoints in program text
 */
void
_install_brkpts()
{
	register struct brkpt_table *ebt;
	volatile struct brkpt_table *bt;
	jmp_buf bp_buf;
	extern int *nofault;
	extern int _kernel_bp[];

	for (bt = brkpt_table; bt < &brkpt_table[NBREAKPOINTS]; bt++) {
		if (bt->bt_type == BTTYPE_EMPTY)
			continue;

		if (setjmp(bp_buf)) {
			/* ugh!! fault of some sort, undo the damage */
			sa_spl();
			for (ebt = brkpt_table; ebt < bt; ebt++) {
				if (ebt->bt_type == BTTYPE_EMPTY)
					continue;
				if (ebt->bt_type != BTTYPE_SUSPEND)
					_set_memory(ebt->bt_addr, SW_WORD,
					   ebt->bt_inst);
				/* delete all temporary brkpts */
				if (ebt->bt_type == BTTYPE_TEMP
				    || ebt->bt_type == BTTYPE_CONT)
					ebt->bt_type = BTTYPE_EMPTY;
				/* reinstate suspended breakpoints */
				if (ebt->bt_type == BTTYPE_SUSPEND)
					ebt->bt_type = ebt->bt_oldtype;
			}
			bt->bt_type = BTTYPE_EMPTY;
			_dbg_error("bad breakpoint address: 0x%x", bt->bt_addr);
			/* doesn't return */
		} else {
			nofault = bp_buf;
			if (bt->bt_type != BTTYPE_SUSPEND) {
				if (is_rom(bt->bt_addr)) {
				    printf("Warning: cannot create breakpoint at 0x%x\n",
					 bt->bt_addr);
				    nofault = 0;
				    continue;
				}
				bt->bt_inst = _get_memory(bt->bt_addr, SW_WORD);
				_set_memory(bt->bt_addr, SW_WORD, *_kernel_bp);
			}
			nofault = 0;
		}
	}
}

/*
 * remove_brkpts -- remove breakpoints for program text
 */
static int bptype_save;		/* bp type saved here for _bp_fault() */

void
_remove_brkpts()
{
	register struct brkpt_table *bt;
	register unsigned bp_pc;

	bp_pc = _regs[R_CAUSE] & CAUSE_BD ? _regs[R_EPC] + 4 : _regs[R_EPC];
	bptype_save = lookup_bptype(bp_pc);
	for (bt = brkpt_table; bt < &brkpt_table[NBREAKPOINTS]; bt++) {
		if (bt->bt_type == BTTYPE_EMPTY)
			continue;

		if (bt->bt_type != BTTYPE_SUSPEND) {
			if (is_rom(bt->bt_addr)) continue;	
			_set_memory(bt->bt_addr, SW_WORD, bt->bt_inst);
		}
		/*
		 * if we're really stopping, delete all temporary brkpts 
		 * delete "continue" breakpoints, since we must be past
		 * them
		 */
		if ((bt->bt_type == BTTYPE_TEMP && bptype_save != BTTYPE_CONT)
		    || bt->bt_type == BTTYPE_CONT)
			bt->bt_type = BTTYPE_EMPTY;
		/* reinstate suspended breakpoints */
		if (bt->bt_type == BTTYPE_SUSPEND)
			bt->bt_type = bt->bt_oldtype;
	}
}

/*
 * _clear_brkpts -- clear breakpoint table
 * Should be called if a new image is loaded, etc which
 * invalidates current breakpoints.
 */
void
_clear_brkpts()
{
	register struct brkpt_table *bt;

	for (bt = brkpt_table; bt < &brkpt_table[NBREAKPOINTS]; bt++)
		bt->bt_type = BTTYPE_EMPTY;
}

/*
 * lookup_bptype -- search breakpoint table to determine type of
 * breakpoint at pc
 */
static int
lookup_bptype(pc)
unsigned pc;
{
	register struct brkpt_table *bt;

	for (bt = brkpt_table; bt < &brkpt_table[NBREAKPOINTS]; bt++)
		if (bt->bt_type != BTTYPE_EMPTY && bt->bt_addr == pc)
			return(bt->bt_type);

	return(BTTYPE_EMPTY);
}

/*
 * _bp_fault -- special purpose fault handling for breakpoints
 * transferred to by general fault handler
 */
void
_bp_fault()
{
	int regstyle;
	extern char *getenv();
	extern char *atob();

	atob(getenv("regstyle"), &regstyle);

	switch (bptype_save) {
	case BTTYPE_TEMP:
		_PrintInstruction(_regs[R_EPC], regstyle, 1);
		if (--step_count > 0) {
			(*step_routine)(BTTYPE_TEMP);
			/* doesn't return */
		}
		break;

	case BTTYPE_EMPTY:
		printf("Unexpected breakpoint\n");
		_PrintInstruction(_regs[R_EPC], regstyle, 1);
		_regs[R_EPC] += 4;
		_PrintInstruction(_regs[R_EPC], regstyle, 1);
		break;

	case BTTYPE_PERM:
		printf("Breakpoint\n");
		_PrintInstruction(_regs[R_EPC], regstyle, 1);
		break;

	case BTTYPE_CONT:
		_resume();
		/* doesn't return */

	default:
	case BTTYPE_SUSPEND:
		_fatal_error("_bp_fault");
		/* doesn't return */
	}
	_restart();
}

/*
 * is_branch -- determine if instruction can branch
 */
static
is_branch(i)
union mips_instruction i;
{
	switch (i.j_format.opcode) {
	case spec_op:
		switch (i.r_format.func) {
		case jr_op:
		case jalr_op:
			return(1);
		}
		return(0);

	case bcond_op:
	case j_op:	case jal_op:	case beq_op:
	case bne_op:	case blez_op:	case bgtz_op:
		return(1);

	case cop0_op:
	case cop1_op:
	case cop2_op:
	case cop3_op:
		switch (i.r_format.rs) {
		case bc_op:
			return(1);
		}
		return(0);
	}
	return(0);
}

/*
 * is_jal -- determine if instruction is jump and link
 */
static
is_jal(i)
union mips_instruction i;
{
	switch (i.j_format.opcode) {
	case spec_op:
		switch (i.r_format.func) {
		case jalr_op:
			return(1);
		}
		return(0);

	case jal_op:
		return(1);

	case bcond_op:
		switch (i.i_format.rt) {
		case bltzal_op:
		case bgezal_op:
			return(1);
		}
		return(0);
	}
	return(0);
}

/*
 * is_brkpt -- check is a breakpoint is set at pc
 */
static
is_brkpt(pc)
unsigned pc;
{
	register struct brkpt_table *bt;

	for (bt = brkpt_table; bt < &brkpt_table[NBREAKPOINTS]; bt++) {
		switch (bt->bt_type) {

		case BTTYPE_TEMP:
		case BTTYPE_PERM:
		case BTTYPE_CONT:
			if (bt->bt_addr == pc)
				return(1);
			break;

		case BTTYPE_SUSPEND:
		case BTTYPE_EMPTY:
			break;

		default:
			_fatal_error("is_brkpt");
			/* doesn't return */
		}
	}
	return(0);
}

/*
 * is_conditional -- determine if instruction is conditional branch
 */
static
is_conditional(i)
union mips_instruction i;
{
	switch (i.j_format.opcode) {
	case bcond_op:
	case beq_op:
	case bne_op:
	case blez_op:
	case bgtz_op:
		return(1);

	case cop0_op:
	case cop1_op:
	case cop2_op:
	case cop3_op:
		switch (i.r_format.rs) {
		case bc_op:
			return(1);
		}
		return(0);
	}
	return(0);
}

/*
 * branch_target -- calculate branch target
 */
static unsigned
branch_target(i, pc)
union mips_instruction i;
unsigned pc;
{
	register short simmediate;

	switch (i.j_format.opcode) {
	case spec_op:
		switch (i.r_format.func) {
		case jr_op:
		case jalr_op:
			return(_regs[i.r_format.rs]);
		}
		break;

	case j_op:
	case jal_op:
		return( ((pc+4)&~((1<<28)-1)) | (i.j_format.target<<2) );

	case bcond_op:
	case beq_op:
	case bne_op:
	case blez_op:
	case bgtz_op:
		/*
		 * assign to temp since compiler currently
		 * doesn't handle signed bit fields
		 */
		simmediate = i.i_format.simmediate;
		return(pc+4+(simmediate<<2));

	case cop0_op:
	case cop1_op:
	case cop2_op:
	case cop3_op:
		switch (i.r_format.rs) {
		case bc_op:
			/*
			 * kludge around compiler deficiency
			 */
			simmediate = i.i_format.simmediate;
			return(pc+4+(simmediate<<2));
		}
		break;
	}
	_fatal_error("branch_target");
}

/*
 * suspend_brkpt -- temporarily suspend breakpoint in order to continue
 */
static void
suspend_brkpt(pc)
unsigned pc;
{
	register struct brkpt_table *bt;

	for (bt = brkpt_table; bt < &brkpt_table[NBREAKPOINTS]; bt++) {
		if (bt->bt_type == BTTYPE_EMPTY || bt->bt_addr != pc)
			continue;

		switch (bt->bt_type) {
		case BTTYPE_PERM:
		case BTTYPE_TEMP:
			bt->bt_oldtype = bt->bt_type;
			bt->bt_type = BTTYPE_SUSPEND;
			break;

		case BTTYPE_CONT:
		case BTTYPE_SUSPEND:
			_fatal_error("suspend_brkpt");
			/* doesn't return */
		}
	}
}

/*
 * _check_bp -- called from saio lib fault handler to allow debugger
 * a crack at exceptions
 */
_check_bp()
{
	register int *jb_ptr;
	extern unsigned _mode_save;
	extern unsigned _exc_save;
	extern unsigned _cause_save;

	if (_mode_save == MODE_CLIENT
	  && (_exc_save == EXCEPT_BRKPT
	        || (_exc_save == EXCEPT_NORM
	              && (_cause_save & CAUSE_EXCMASK) == EXC_BREAK
		   )
	     )
	) {
		if (bp_nofault) {
			/*
			 * remote debug protocol wants to catch this one
			 */
			jb_ptr = bp_nofault;
			bp_nofault = 0;
			longjmp(jb_ptr, 1);
		}
		_bp_fault();	/* special handling for client breakpoints */
		/* doesn't return */
	}
}


/*************************************************
*  is_rom - return true if pc is inside a rom    *
*  area, false otherwise                         *
*************************************************/
static is_rom(pc)
unsigned pc;
{
    	if ((pc & 0xfff00000) == 0xbfc00000) return 1;
	else return 0;
}
