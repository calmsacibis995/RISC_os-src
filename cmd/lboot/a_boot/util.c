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
#ident	"$Header: util.c,v 1.4.2.2 90/05/09 16:19:26 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	<stdio.h>
# include	"mkboot.h"
# include	<ctype.h>

/*
 *	check_master( master ) - do some basic sanity checking for the
 *				 master file entry; the following table
 *				 shows the valid flag combinations
 *
 *                R                               N
 *                E                   F   F       O               K
 *                Q           B       U   U       T   F           E
 *                A   T       L   C   N   N   S   A   S           R
 *                D   T   R   O   H   D   M   O   D   T           N
 *                D   Y   E   C   A   R   O   F   R   Y           E
 *                R   S   Q   K   R   V   D   T   V   P           L
 *               (a) (t) (r) (b) (c) (f) (m) (s) (x) (j) ( ) ( ) (k)
 *              +---+---+---+---+---+---+---+---+---+---+---+---+---+
 *  ONCE    (o) | # | # | # | # | # | # | O | O | O |   |   |   | N |
 *              +---+---+---+---+---+---+---+---+---+---+---+---+---+
 *  REQADDR (a)     | # | # | Y | Y | Y | Y | N | Y |   |   |   | N |
 *              ----+---+---+---+---+---+---+---+---+---+---+---+---+
 *  TTYS    (t)         | # | # | O | O | # | # | # |   |   |   | N |
 *              --------+---+---+---+---+---+---+---+---+---+---+---+
 *  REQ     (r)             | # | # | # | O | O | O |   |   |   | N |
 *              ------------+---+---+---+---+---+---+---+---+---+---+
 *  BLOCK   (b)                 | Y | Y | Y | Y | N |   |   |   | N |
 *              ----------------+---+---+---+---+---+---+---+---+---+
 *  CHAR    (c)                     | N | Y | Y | N |   |   |   | N |
 *              --------------------+---+---+---+---+---+---+---+---+
 *  FUNDRV  (f)                         | Y | Y | N |   |   |   | N |
 *              ------------------------+---+---+---+---+---+---+---+
 *  FUNMOD  (m)                             | Y | N |   |   |   | N |
 *              ----------------------------+---+---+---+---+---+---+
 *  SOFT    (s)                                     |   |   |   | N |
 *              ------------------------------------+---+---+---+---+
 *  FSTYP   (j)                                         |   |   | N |
 *              ----------------------------------------+---+---+---+
 *               (a) (t) (r) (b) (c) (f) (m) (s) (x) (j) ( ) ( ) (k)
 *  
 *  
 *              O: the flag is only valid in this combination
 *              Y: the flag combination is valid
 *              N: the flag combination is not valid
 *              #: not applicable
 *
 * For example, BLOCK is valid with CHAR, FUNDRV, FUNMOD and SOFT, but not valid
 * with NOTADRV.  As another example, ONCE may only be used with FUNMOD,
 * SOFT or NOTADRV; all other flags are immaterial with respect to ONCE.
 *
 * If everything is OK, then return TRUE; otherwise return FALSE if the object
 * file should be skipped.
 */
 boolean
check_master( name, mp )
char *name;
register struct master *mp;
{
	boolean skip = FALSE;

	if ( mp->flag & KOBJECT ) {
		/*
		 * Kernel is handled a little differently
		 */
		if ( mp->flag & ~KOBJECT ) {
			warn( "%s: illegal flags -- no flags allowed for kernel object files", name );
			mp->flag = KOBJECT;
			}

		if ( mp->prefix[0] || (mp->soft && mp->soft != DONTCARE) || mp->ndev ) {
			warn( "%s: , prefix, major and devices ignored", name );
			strncpy( mp->prefix, "", sizeof(mp->prefix) );
			mp->soft = 0;
			mp->ndev = 0;
			}

		if ( mp->ndep > 0 )
			{
			warn( "%s: dependencies not applicable for kernel", name );
			mp->ndep = 0;
			}

		if ( mp->nrtn > 0 )
			{
			warn( "%s: routine definitions not applicable for kernel", name );
			mp->nrtn = 0;
			}
		}

	if ( mp->flag & TTYS && ! (mp->flag & (CHAR|FUNDRV)) )
		{
		warn( "%s: illegal flag combination -- TTYS only valid with CHAR|FUNDRV", name );
		skip = TRUE;
		}

	if ( (mp->flag & (BLOCK|CHAR|FUNDRV|FUNMOD|SOFT)) && (mp->flag & NOTADRV) )
		{
		warn( "%s: illegal flag combination -- BLOCK|CHAR|FUNDRV|FUNMOD|SOFT mutually exclusive with NOTADRV", name );
		skip = TRUE;
		}

	if ( (mp->flag & CHAR) && (mp->flag & FUNDRV) )
		{
		warn( "%s: illegal flag combination -- CHAR mutually exclusive with FUNDRV", name );
		skip = TRUE;
		}

	if ( (mp->flag & REQADDR) && (mp->flag & SOFT) )
		{
		warn( "%s: illegal flag combination -- REQADDR not valid with SOFT", name );
		skip = TRUE;
		}

	if ( (mp->flag & ONCE) && ! (mp->flag & (SOFT|NOTADRV|FUNMOD)) )
		{
		warn( "%s: illegal flag combination -- ONCE only valid for SOFT|NOTADRV|FUNMOD", name );
		skip = TRUE;
		}

	if ( (mp->flag & REQ) && ! (mp->flag & (SOFT|NOTADRV|FUNMOD)) )
		{
		warn( "%s: illegal flag combination -- REQ only valid for SOFT|NOTADRV|FUNMOD", name );
		skip = TRUE;
		}

	if ( ((unsigned)mp->soft > 127) && (mp->soft != DONTCARE) )
		{
		warn( "%s: major number cannot exceed 127", name );
		skip = TRUE;
		}

	if ( mp->flag & (NOTADRV|FUNMOD) && ( mp->ndev ) )
		{
		warn( "%s: #DEV field not applicable for NOTADRV|FUNMOD", name );
		skip = TRUE;
		}

	return( skip? FALSE : TRUE );
}

/*
 *	lcase(str) - convert all upper case characters in 'str' to lower
 *			case and return pointer to start of string
 */

char *
lcase(str)
char	*str;
{
	register char	*ptr;

	for (ptr = str; *ptr != '\0'; ++ptr)
		if (isascii(*ptr) && isupper(*ptr))
			*ptr = tolower(*ptr);
	return( str );
}

/*
 *	ucase(str) - convert all lower case characters in 'str' to upper
 *			case and return pointer to start of string
 */

char *
ucase(str)
char	*str;
{
	register char	*ptr;

	for (ptr = str; *ptr != '\0'; ++ptr)
		if (isascii(*ptr) && islower(*ptr))
			*ptr = toupper(*ptr);
	return( str );
}

/*
 *	basename(path) - obtain the file name associated with the (possibly)
 *			 full pathname argument 'path'
 */

char *
basename(path)
char	*path;
{
	char	*retval;

	/* return the entire name if no '/'s are found */
	retval = path;

	while (*path != NULL)
		if (*path++ == '/')
			retval = path;	/* now path points past the '/' */
		
	return( retval );
}

/*
 * Copy a string to the string table; handle all of the C language escape
 * sequences with the exception of \0
 */
 char *
copystring( text )
register char *text;
{
	register char c;
	char *start;
	int count, byte;

	if ( nstring-string > MAXSTRING-strlen(text)-1 )
		yyfatal( "string table overflow" );

	start = nstring--;

	while( *text ) {
		if ( (*++nstring = *text++) == '\\' ) {
			switch( c = *text++ )
			{
			default:
				*nstring = c;
				break;
			case 'n':
				*nstring = '\n';
				break;
			case 'v':
				*nstring = '\v';
				break;
			case 't':
				*nstring = '\t';
				break;
			case 'b':
				*nstring = '\b';
				break;
			case 'r':
				*nstring = '\r';
				break;
			case 'f':
				*nstring = '\f';
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				for( count=0,byte=c-'0'; ++count<3 && ((c=(*text))>='0'&&c<='7'); ++text )
					byte = 8 * byte + c - '0';
				if ( byte == 0 )
					yyfatal( "\\0 illegal within string" );
				if ( byte > 255 )
					yyfatal( "illegal character escape: \\%o", byte );
				*nstring = byte;
				break;
			}
		}
	}

	*++nstring = 0;
	++nstring;

	return( start );
}

/*
 * Read with error checking
 */
 int
myread( fildes, buf, nbyte, fname )
int fildes;
char *buf;
register unsigned nbyte;
register char *fname;
{
	register int rbyte;

	if ( (rbyte = read(fildes,buf,nbyte)) == -1 ) { perror( fname );
		return( 1 );
	} else
		if ( rbyte != nbyte ) {
			warn( "%s: truncated read", fname );
			return( 1 );
		} else
			return( 0 );
}

/*
 * Write with error checking
 */
 int
mywrite( fildes, buf, nbyte, fname )
int fildes;
char *buf;
register unsigned nbyte;
register char *fname;
{
	register int wbyte;

	if ( (wbyte = write(fildes,buf,nbyte)) == -1 ) {
		perror( fname );
		return( 1 );
	} else
		if ( wbyte != nbyte ) {
			warn( "%s: truncated write", fname );
			return( 1 );
		} else
			return( 0 );
}

/*
 * Lseek with error checking
 */
 int
mylseek( fildes, seekoff, whence, fname )
int fildes;
long seekoff;
int whence;
char *fname;
{

	if ( lseek(fildes,seekoff,whence) == -1L ) {
		perror( fname );
		return( 1 );
	} else
		return( 0 );
}

/*
 * malloc() with error checking
 */
 char *
mymalloc( size )
{
	char *p = malloc(size);
	if (p == (char *) NULL) {
		fprintf("no memory\n");
		exit(2);
	}
	return(p);
}

/*
 * Print_master( name, master )
 *
 * Print the master structure
 */
 void
print_master( name, master )
char *name;
struct master *master;
{
	register struct depend *dp;
	register struct routine *rp;
	register struct variable *vp;
	register struct format *fp;
	register int i, j;


	printf( "%s:\n", name );

	printf( "     flag=" );
	printf( "%s\n", print_flag(master->flag) );
	printf( "     prefix=%-30s  major=%-3d  ndev=%-2d\n",
		master->prefix, master->soft, master->ndev);

	if ( master->ndep ) {
		dp = (struct depend *) POINTER( master->o_depend, master );
		for( i=0; i<master->ndep; ++i, ++dp )
			printf( "     dependency=%s\n", POINTER(dp->name,master) );
	}

	if ( master->nrtn ) {
		static char *id[] = { "", "nosys", "nodev", "true", "false", "fsnull", "fsstray", "nopkg", "noreach" };
		rp = (struct routine *) POINTER( master->o_routine, master );
		for( i=0; i<master->nrtn; ++i, ++rp )
			printf( "     routine %s(){%s}\n", POINTER(rp->name,master), id[rp->id] );
	}

}


/*
 * print_flag( flag )
 *
 * Print the flags symbolically
 */
 char *
print_flag( flag )
register unsigned short flag;
{
	static char buffer[256];

	strcpy( buffer, "" );

	if ( flag == 0 )
		return( "none" );

	if ( flag & KOBJECT )
		strcat( buffer, ",KERNEL" );

	if ( flag & ONCE )
		strcat( buffer, ",ONCE" );

	if ( flag & REQ )
		strcat( buffer, ",REQ" );

	if ( flag & BLOCK )
		strcat( buffer, ",BLOCK" );

	if ( flag & CHAR )
		strcat( buffer, ",CHAR" );

	if ( flag & REQADDR )
		strcat( buffer, ",REQADDR" );

	if ( flag & TTYS )
		strcat( buffer, ",TTYS" );

	if ( flag & SOFT )
		strcat( buffer, ",SOFT" );

	if ( flag & NOTADRV )
		strcat( buffer, ",NOTADRV" );

	if ( flag & FUNDRV )
		strcat( buffer, ",FUNDRV" );

	if ( flag & FUNMOD )
		strcat( buffer, ",FUNMOD" );

	if ( flag & FSTYP ) 
		strcat(buffer, ",FSTYP");

	return( buffer+1 );
}
