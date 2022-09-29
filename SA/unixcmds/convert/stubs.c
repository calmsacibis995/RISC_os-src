#ident "$Header: stubs.c,v 1.1.11.1 90/07/18 16:43:35 huang Exp $"
/* $Header: stubs.c,v 1.1.11.1 90/07/18 16:43:35 huang Exp $ */
/* $Log:	stubs.c,v $
 * Revision 1.1.11.1  90/07/18  16:43:35  huang
 * Branch GENESIS_BETA1 off of trunk.
 * 
 * Revision 1.1  87/08/18  16:53:03  mdove
 * Initial revision
 *  */

/*LINTLIBRARY*/
#include <stdio.h>

/*VARARGS*/
extern printf();

static count = 3;

/*ARGSUSED*/
int StubObjRead (buffer, length) 
	char *buffer;
	int length;
{

	printf("Object Read Invoked\n");
	return count--;
}
int StubObjClose () {

	printf("Object Close Invoked\n");
	return 0;
}
int StubObjInitialize () {

	printf("Object get_info Invoked\n");
	return 0;
}
int StubFmtInitialize () {

	printf("Object get_info Invoked\n");
	return 0;
}
int StubFmtTerminate () {

	printf("Object get_info Invoked\n");
	return 0;
}
/*ARGSUSED*/
int StubFmtConvert (buffer, length) 
	char *buffer;
	int length;
{

	printf("Format convert Invoked\n");
	return 0;
}
int StubFmtWrite (buffer, length) 
	int length;
	char *buffer;
{
	while ( length-- > 0 )
		printf("%02x", *buffer++);
	printf( "\n");

	return 0;
}
int StubFmtClose () {

	printf("Format close Invoked\n");
	return 0;
}
