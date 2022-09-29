#ident "$Header: exec.c,v 1.3 90/02/28 22:07:53 chungc Exp $"
/* $Copyright$ */

#include "saio/stringlist.h"
#include "prom/entrypt.h"

static struct string_list exec_argv;
/* static struct string_list exec_environ;	*/
struct string_list exec_environ;

#define	RETURN	0	/* initiate should return to exec'er */
#define	EXIT	1	/* initiate should exit to prom */

/*
 * execve --  load a new image and transfer to it
 * passes an arglist and environment
 * NO POSSIBILITY OF RETURN
 */
execve(path, argv, envp)
char *path;
char **argv;
char **envp;
{
	register char **wp;
	struct promexec_args pa;
	int (*init_pc)();
	extern (*_prom_exec())();

	init_str(&exec_argv);
	for (wp = argv; wp && *wp; wp++)
		if (new_str1(*wp, &exec_argv))
			return(-1);
	init_str(&exec_environ);
	for (wp = envp; wp && *wp; wp++)
		if (new_str1(*wp, &exec_environ))
			return(-1);

	printf("Execing %s\n", path);
	pa.pa_bootfile = path;
	pa.pa_argc = exec_argv.strcnt;
	pa.pa_argv = exec_argv.strptrs;
	pa.pa_environ = exec_environ.strptrs;
	pa.pa_flags = 0;
	_prom_exec(&pa);
	return(-1);
}

/*
 * rexecve -- load a process image and transfer control to it
 * passes arglist and environment
 * arranges for return of control to original exec'ing process
 * WARNING: loaded process must not overlay exec'ing process
 */
rexecve(path, argv, envp)
char *path;
char **argv;
char **envp;
{
	register char **wp;
	int retval;
	struct promexec_args pa;
	int (*init_pc)();
	int (*bp_addr)();
	extern (*_prom_exec())();

	init_str(&exec_argv);
	for (wp = argv; wp && *wp; wp++)
		if (new_str1(*wp, &exec_argv))
			return(-1);
	init_str(&exec_environ);
	for (wp = envp; wp && *wp; wp++)
		if (new_str1(*wp, &exec_environ))
			return(-1);

	printf("Execing %s\n", path);
	pa.pa_bootfile = path;
	pa.pa_argc = exec_argv.strcnt;
	pa.pa_argv = exec_argv.strptrs;
	pa.pa_environ = exec_environ.strptrs;
	pa.pa_flags = EXEC_NOGO;
	/* preserve current breakpoint handler across execs */
	bp_addr = ((struct restart_blk *)RESTART_ADDR)->rb_bpaddr;
	init_pc = _prom_exec(&pa);
	((struct restart_blk *)RESTART_ADDR)->rb_bpaddr = bp_addr;
	if ((int)init_pc == -1)
		return(-1);
	retval = initiate(exec_argv.strcnt, exec_argv.strptrs,
	    exec_environ.strptrs, init_pc, RETURN);
	return(retval);
}
