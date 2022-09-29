#include "saio/saioctl.h"
#include "sys/stdio.h"
#include "sys/file.h"
#include "varargs.h"


/* fake out stdio calls */
fprintf(fd, fmt, va_alist)
int fd;
char *fmt;
va_dcl
{

	FILE _strbuf;
    	char buf[1024];
	va_list ap;

	va_start(ap);
/*	_strbuf._flag = _IOWRT+_IOSTRG; */
	_strbuf._ptr = buf;
	_strbuf._cnt = 32767;
	_doprnt(fmt, ap, &_strbuf);
	stdio_putc('\0', &_strbuf);
    	write(fd, buf, strlen(buf));		/* XXX tab expansion, crnl */
} /* fprintf */


fopen(name, how)
char *name;
char *how;
{
    int flags, result;
    
    if (strcmp(how, "r") == 0)
	flags = O_RDONLY;
    else if (strcmp(how, "w") == 0)
	flags = O_WRONLY;
    else if (strcmp(how, "r+") == 0)
	flags = O_RDWR;
    else
	return NULL;
    result = open(name, flags);
    if (result < 0)
	return NULL;
    return result;

} /* fopen */


fclose(fd)
int fd;
{
    return close (fd);
} /* fclose */
