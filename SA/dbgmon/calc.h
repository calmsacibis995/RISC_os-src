#ident "$Header: calc.h,v 1.2 90/01/11 13:59:45 huang Exp $"
/* $Copyright$ */

/*
 * $Log:	calc.h,v $
 * Revision 1.2  90/01/11  13:59:45  huang
 * Added $Copyright$
 * 
 * Revision 1.1  87/08/18  13:52:23  mdove
 * Initial revision
 * 
 * Revision 1.1  86/06/12  09:54:13  mcneal
 * Initial revision
 * 
 */
#define STKSIZ 10
#define CLRLINE "\r                                       \r"
#define CTRL(x) ('x'&037)

struct func_t {
	int	f_inp;
	int	(*f_func)();
};
