#ident "$Header: prom_entry.s,v 1.4 90/03/07 09:54:12 hawkes Exp $"
/*	%Q%	%I%	%M% */
/* $Copyright$ */

/*
 * prom_entry.s -- interface to prom entry points
 */

#include "machine/regdef.h"
#include "machine/asm.h"
#include "machine/cpu.h"
#include "prom/entrypt.h"

/*
 * Prom entry points
 */

/*
 * Return control to prom entry points
 *
 * RESET	transferred to on hardware reset, configures MIPS boards,
 *		runs diags, check for appropriate auto boot action in
 *		"bootmode" environment variable and performs that action.
 *
 * EXEC		called to utilize prom to boot new image.  After the booted
 *		program returns control can either be returned to the
 *		original caller of the exec routine or to the prom monitor.
 *		(to return to the original caller, the new program must
 *		not destroy any text, data, or stack of the parent.  the
 *		new programs stack continues on the parents stack.
 *
 * RESTART	re-enter the prom command parser, do not reset prom state
 *
 * REINIT	reinitialize prom state and re-enter the prom command parser
 *
 * REBOOT	check for appropriate bootmode and perform, no configuration
 *		or diags run
 *
 */
LEAF(_prom_autoboot)
XLEAF(prom_autoboot)
	li	v0,PROM_AUTOBOOT
	j	v0
	END(_prom_autoboot)

LEAF(_prom_reset)
XLEAF(prom_reset)
	li	v0,PROM_RESET
	j	v0
	END(_prom_reset)

LEAF(_prom_exec)
XLEAF(prom_exec)
	li	v0,PROM_EXEC
	j	v0
	END(_prom_exec)

LEAF(_prom_restart)
XLEAF(prom_restart)
	li	v0,PROM_RESTART
	j	v0
	END(_prom_restart)

LEAF(_prom_reinit)
XLEAF(prom_reinit)
	li	v0,PROM_REINIT
	j	v0
	END(_prom_reinit)

LEAF(_prom_reboot)
XLEAF(prom_reboot)
	li	v0,PROM_REBOOT
	j	v0
	END(_prom_reboot)
/*
 * these routines access prom "stdio" routines, and may be used
 * by standalone programs that would like to use prom io redirection
 */
LEAF(_prom_getchar)
	li	v0,PROM_GETCHAR
	j	v0
	END(_prom_getchar)

LEAF(_prom_putchar)
	li	v0,PROM_PUTCHAR
	j	v0
	END(_prom_putchar)

LEAF(_prom_gets)
	li	v0,PROM_GETS
	j	v0
	END(_prom_gets)

LEAF(_prom_puts)
	li	v0,PROM_PUTS
	j	v0
	END(_prom_puts)

LEAF(_prom_printf)
	li	v0,PROM_PRINTF
	j	v0
	END(_prom_printf)

LEAF(prom_getenv)
	li	v0,PROM_GETENV
	j	v0
	END(prom_getenv)

LEAF(prom_setenv)
	li	v0,PROM_SETENV
	j	v0
	END(prom_setenv)

/*
 * read-modify-write routine use special cpu board circuitry to accomplish
 * vme bus r-m-w cycles.  all routines are similar to:
 *	unsigned char
 *	orb_rmw(addr, mask)
 *	unsigned char *addr;
 *	unsigned mask;
 *	{
 *		register unsigned rval;
 *
 *		lockbus();
 *		rval = *addr;
 *		*addr = rval & mask;
 *		unlockbus();
 *		return(rval);
 *	}
 */
LEAF(orw_rmw)
	li	v0,PROM_ORW_RMW
	j	v0
	END(orw_rmw)

LEAF(orh_rmw)
#ifdef SABLE
	lh	v0,(a0)
	or	v0,a1
	sh	v0,(a0)
	j	ra
#endif
	li	v0,PROM_ORH_RMW
	j	v0
	END(orh_rmw)

LEAF(orb_rmw)
	li	v0,PROM_ORB_RMW
	j	v0
	END(orb_rmw)

LEAF(andw_rmw)
	li	v0,PROM_ANDW_RMW
	j	v0
	END(andw_rmw)

LEAF(andh_rmw)
#ifdef SABLE
	lh	v0,(a0)
	and	v0,a1
	sh	v0,(a0)
	j	ra
#endif
	li	v0,PROM_ANDH_RMW
	j	v0
	END(andh_rmw)

LEAF(andb_rmw)
	li	v0,PROM_ANDB_RMW
	j	v0
	END(andb_rmw)

/*
 * prom saio entry points
 * (mainly for implementing stdin/stdout for standalones)
 */
LEAF(_prom_open)
	li	v0,PROM_OPEN
	j	v0
	END(_prom_open)

LEAF(_prom_read)
	li	v0,PROM_READ
	j	v0
	END(_prom_read)

LEAF(_prom_write)
	li	v0,PROM_WRITE
	j	v0
	END(_prom_write)

LEAF(_prom_ioctl)
	li	v0,PROM_IOCTL
	j	v0
	END(_prom_ioctl)

LEAF(_prom_close)
	li	v0,PROM_CLOSE
	j	v0
	END(_prom_close)
