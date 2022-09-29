#ident "$Header: convert.c,v 1.5.4.1 90/07/18 16:42:51 huang Exp $"
/* $Header: convert.c,v 1.5.4.1 90/07/18 16:42:51 huang Exp $ */
/* $Log:	convert.c,v $
 * Revision 1.5.4.1  90/07/18  16:42:51  huang
 * Branch GENESIS_BETA1 off of trunk.
 * 
 * Revision 1.5  90/05/14  14:11:01  lian
 * Bug fix of -b option in converting multi-files.
 * 
 * Revision 1.4  90/05/03  11:04:04  lian
 * Enhanced to support multiple files.
 * Added -p option (padding data in address gaps of two files).
 * 
 * Revision 1.3  90/03/01  12:00:32  chungc
 * added usage message.
 * added -i option (swap byte for 16 bit proms).
 * 
 * Revision 1.2  88/02/09  00:52:18  mdove
 * Marks changes
 * 
 * Revision 1.1  87/08/18  16:52:53  mdove
 * Initial revision
 *  */

/*******************************************************************************
 *
 *	convert - Convert object files to downloadable format.
 *
 *  convert -f <format> -b <byte number> file ...
 *
 *	where <format> is either "intelhex", "step" or "srec"
 *
 * This program converts BSD4.2 a.out or mips object files to downloadable
 * format suitable for the prom programmers.  The program is designed to
 * be easily expanadble for other output formats.
 *
 ******************************************************************************/

#define LANGUAGE_C 1
#define mips 1
#define S3_DATA_RECORD 0x03

#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include "a.out.h"
#include "sex.h"
#include "convert.h"
#include "stubs.h"
#include "mips.h"
#include "intel_hex.h"
#include "s_rec.h"

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];
extern char *malloc();

char bad_flag[] = "%s: %c is not a valid flag.\n";
char *progname;
char *objname;
 
/*
 * Object file type control structures
 */

struct object_file_type_table_descriptor object_file_type_table[] = {

	MIPSEBMAGIC,	MIPS,		/* headers: BE - target: BE */
	MIPSELMAGIC,	MIPS,		/* headers: LE - target: LE */
	SMIPSEBMAGIC,	MIPS,		/* headers: LE - target: BE */
	SMIPSELMAGIC,	MIPS,		/* headers: BE - target: LE */

	OMAGIC, BERKELEY,
	NMAGIC, BERKELEY,
	ZMAGIC, BERKELEY,

	0x0550, COFF,
	0x0551, COFF,
	0x0570, COFF,
	0x0575, COFF
};
#define N_MAGICS (sizeof( object_file_type_table) /  \
		  sizeof(struct object_file_type_table_descriptor) )

struct object_file_descriptor object_files[] = {

	BERKELEY, StubObjInitialize, StubObjRead, StubObjClose,
	COFF, StubObjInitialize, StubObjRead, StubObjClose,
	MIPS, MipsInitialize, MipsRead, MipsClose
};

#define N_OBJECT_TYPES (sizeof(object_files)/sizeof(struct object_file_descriptor))

struct object_file_descriptor *object;

/*
 * Output format control structures
 */

char *format_names[] = { "intelhex", "step", "srec", "s1rec","s2rec", "s3rec", 0 };
#define N_FORMATS ((sizeof( format_names ) / sizeof( char *)) )

struct format_descriptor formats[ N_FORMATS ] = {

	16,
	IntelhexInitialize,
	IntelhexConvert,
	IntelhexWrite,
	IntelhexTerminate,

	4,
	IntelhexInitialize,
	StepConvert,
	IntelhexWrite,
	IntelhexTerminate,

	16,
	S2RecordInitialize,
	SRecordConvert,
	SRecordWrite,
	SRecordTerminate,

	16,
	S1RecordInitialize,
	SRecordConvert,
	SRecordWrite,
	SRecordTerminate,

	16,
	S2RecordInitialize,
	SRecordConvert,
	SRecordWrite,
	SRecordTerminate,

	16,
	S3RecordInitialize,
	SRecordConvert,
	SRecordWrite,
	SRecordTerminate,

	16,
	StubFmtInitialize,
	StubFmtConvert,
	StubFmtWrite,
	StubFmtTerminate,
};

struct format_descriptor *format = formats;

int byte_number = -1;		/* which byte of n bit word */
int skip = -1;			/* bytes to skip in file before outputing */
int count = -1;			/* max bytes to output */

/*
 * Object file section globals
 */
unsigned int data_record;		/* Record format to convert to */

address load_address;			/* Code loaded here */
address start_address;			/* Control passed here on execution */
address record_address = -1;		/* Next record address */

address start_address1 = -1;		/* Start address of object 1 */
address section_end_address = -1;	/* End address+4 of last section converted */
address last_obj_end_address = -1;	/* Previous object's end_address+4 */
address current_obj_start_address;

/*
 *  Sex variables
 */
int my_sex;			/* Host byte sex */
int swap;			/* True if headers and object of opposite sex */

int buffer[ 1024 ];

int byteswap = 0;
int padding  = 0;		/* Option to pad data in program gaps */
unsigned pad_byte;		/* padding byte */

main(argc,argv)

	int argc;
	char **argv;

{

	register int i = 1;
	register int j;
	char *cp;
	int status;

	/* Initialize */
	my_sex = gethostsex();
	progname = argv[ 0 ];

	if (argc < 2) {
	    fprintf(stderr, "Usage: convert [-f <format>] [-b <byte number>] [-i] [-p <byte>]\n");
	    fprintf(stderr, "               [-s <starting byte count>] [-c <byte count>] \n");
	    exit(1);
	} 

	/* Parse the flags - Note that all flags must precede all filenames */
	while ( i < argc ) {

		/* No flags - start processing filenames */
		if ( argv[i][0] != '-' ) {
			break;
		}

		/* process each flag */
		switch ( argv[i][1] ) {
		case 'b':
			/* Allow the byte number to be separated from the
			 *   flag by zero or more blanks
			 */
			cp = (argv[i][2] ? &argv[i][2]
				         : argv[ (++i < argc ? i :--i) ]);
			byte_number = atoi( cp );
			break;

		case 'f':
			/* Allow the format specifier to separated from the
			 *   flag by zero or more blanks
			 */
			cp = (argv[i][2] ? &argv[i][2]
				         : argv[ (++i < argc ? i :--i) ]);

			/* Check for legal format specifier */
			if ((j = MatchArg(cp, format_names, IGNORECASE)) >= 0) {
				format = &formats[ j ];
				break;
			}
			else {
				fprintf( stderr,
					 "%s: Invalid format specification.\n",
					 progname );
				fprintf( stderr, "%s: Try ", progname );
				for ( j = N_FORMATS - 1; j >= 0; j-- ) {

					switch ( j ) {
					case 0:
						cp = "%s(default).\n";
						break;
					case 1:
						cp = "%s or ";
						break;
					default:
						cp = "%s, ";
					}

					fprintf(stderr, cp, format_names[ j ]);
				}
				exit( 1 );
			}
		case 's':
			skip = atoi(argv[i]+2);
			break;
		case 'c':
			count = atoi(argv[i]+2);
			break;
		case 'i':		/* swap the bytes in each half word */
			byteswap++;
			break;

		case 'p':		/* pad data in the program gaps */
			padding++;
			/* Allow the padding byte to be separated from the
			 *   flag by zero or more blanks
			 */
			cp = (argv[i][2] ? &argv[i][2] : argv[ (++i < argc ? i :--i) ]);
			pad_byte = ((unsigned)strtol(cp, (char **)NULL, 0) & 0xFF);
			break;

		default:
			fprintf( stderr, bad_flag, progname, argv[ i ][ 1 ] );
			exit(1);
		}

		/* Pick up next command line arg */
		i++;
	}

	/* Process each filename */
	if ( i >= argc ) {

		/* no file arguments - process contents of stdin */
		status = Convert(1);
	}
	else {
		for ( ; i < argc; i++ ) {

			/* Open the next file */
			objname = argv[i];
			if ( freopen( argv[i], "r", stdin ) == NULL ) {

				(void)ErrChk( stdin, 0 );
				exit(2);
			}
			/* Do it */
			if ( status = Convert((i+1)<argc? 0: 1) ) {
				break;
			}
			if (count == 0)
				break;
			last_obj_end_address = section_end_address;
		}
	}

	/* Clean code here */
	(void) fflush(stdout) ; 
	exit( status );

} /* end of main */

int Convert(last_object) 
	int	last_object;
{

	register int error;
	enum object_file_type type;
	int input_blocking_factor;
	int output_blocking_factor;
	int length;

	int	i;
	address gap_length;

	/* Obtain the type file type */
	if ( error = GetObjectType( (int *)&type ) ) return error;
	object = &object_files[ (int)type ];

	/* Obtain blocking factor from output format */
	output_blocking_factor = format->blocking_factor;
	input_blocking_factor = byte_number < 0 ? output_blocking_factor 
						: 4 * output_blocking_factor;

	if (count > 0 && (count % output_blocking_factor) != 0) {
	    fprintf(stderr,"Error: Count must be divisible by blocking factor (%d), pick another count or format\n", output_blocking_factor);
	    return -1;
	}
		
	if (skip > 0 && (skip % output_blocking_factor) != 0) {
	    fprintf(stderr,"Error: Skip count must be divisible by blocking factor (%d), pick another skip count or format\n", output_blocking_factor);
	    return -1;
	}
		
	/* Initialize the output conversion routines */
	if ( error = (*format->initialize)( output_blocking_factor) )
		return error;

	/* Obtain the lengths and addresses of the text and data sections */
	if ( error = (*object->initialize)( input_blocking_factor) ) 
		return error;

	/* Initialize record address */
	if (record_address == -1) {
		record_address = (data_record == S3_DATA_RECORD) ? load_address : 0;
	}

	/* Multi-object conversion: fill the gap with padding data if -p specified */
	if (last_obj_end_address != -1) {

		current_obj_start_address = load_address;
		if (last_obj_end_address > current_obj_start_address) {
			fprintf(stderr, 
				"Warning: %s - start address overlaps preceding object.\n", 
				objname);
			gap_length = 0;
		}
		else
			gap_length = current_obj_start_address - last_obj_end_address;

		if ((gap_length % output_blocking_factor) != 0) {
			fprintf(stderr, "Error: %s - address gap must be divisible by blocking factor (%d bytes)\n", objname, output_blocking_factor);
			return -1;
		}

		if (!padding) {
			if (skip > 0) {
		    		skip -= gap_length;

				/*
			 	 * -s option: 
				 * For s3rec format, always adjust the address.
			 	 * For other formats, start from 0 after the skipped code.
				 *
				 * -b option: address should be incremented only by 1 
				 * for every word.
			 	 */
		    		if (data_record == S3_DATA_RECORD) {
				     record_address += (byte_number < 0 ? gap_length
									: gap_length/4);
				}

				/* adjust count and address if over skipped */
				if (skip < 0) {
					if (count > 0)
						count = ((count += skip) < 0) ? 0:count;

		    			if (data_record != S3_DATA_RECORD) {
					     record_address += (byte_number < 0 ? -skip
										: -skip/4);
					}
				}
			}
			else {
				if (count > 0) 
					count = ((count -= (int)gap_length) < 0) ? 0:count;

				record_address += (byte_number < 0 ? gap_length : gap_length/4);
			}
		}
		else {
			/* Pad data in the program gaps */
			for (i=0; i<input_blocking_factor; i++) {
				buffer[i] = (int)(pad_byte<<24) | (pad_byte<<16) |
					   	 (pad_byte<<8)  | pad_byte;
			}

			for ( ; (length=(current_obj_start_address - last_obj_end_address)) > 0; 
					last_obj_end_address += input_blocking_factor) {

				if (length > input_blocking_factor)
					length = input_blocking_factor;

			 	/* skip: For s3rec format, always adjust the address. */
				if (skip > 0) {
		    			skip -= length;
		    			if (data_record == S3_DATA_RECORD) {
					     record_address += (byte_number < 0 ? length
									 	: length/4);
					}
		    			continue;
				}

				if (count == 0) {
		    			break;
				} else {
		    			count -= length;
				}

				/* Convert a fill pattern record to the output format */
				if ( (error = (*format->convert)(buffer,
						 		 byte_number < 0 ? length
										 : length/4)) )
					return error;
			
				/* Write the record out to the out file */
				if ( error = (*format->write)() )
					return error;
			}
		}
	}

	/* Main processing loop */
	while (( length = (*object->read)(buffer, input_blocking_factor)) > 0) {

		if (skip > 0) {

		    skip -= length;
		    if (data_record == S3_DATA_RECORD) {
			record_address += (byte_number < 0 ? length : length/4);
		    }
		    continue;
		}

		if (count == 0) {
		    break;
		} else {
		    count -= length;
		}

		/* Convert the the last record read to the output format */
		if ( (error = (*format->convert)(buffer,
						 byte_number < 0 ? length
								 : length/4 )) )
			return error;

		/* Write the record out to the out file */
		if ( error = (*format->write)() )
			return error;
	}

	/* Save the start address of object 1 */
	if (start_address1 == -1)
		start_address1 = start_address;

	/* Create only one termination record when converting multiple objects */
	if (last_object || (count == 0)) {
		start_address = start_address1;	
		if ( error = (*format->terminate)() )
			return error;
	}

	return (length < 0) ? length : (*object->close)();

} /* end of Convert */

int GetObjectType( type )
	enum object_file_type *type;
{
	register int i;
	union {
		short number;
		char c;
	} magic;
	int error;

	/* Get the magic number from the object file */
	if ( (error = fread((char *)&magic, sizeof( magic ), 1, stdin)) == 0)
		return ErrChk( stdin, error );

	if ( (error = fseek(stdin, 0L, 0 )) != 0)
		return ErrChk( stdin, error );

	/* find the type of the the object file */
	for ( i = 0; i < N_MAGICS; i++ ) {

		if ( object_file_type_table[ i ].magic == magic.number) {
			*type = object_file_type_table[ i ].type;
			return OK;
		}
	}

	fprintf(stderr,
		"%s: Unrecognized magic number: 0x%x\n",
		progname,
		magic.number);

	return NOT_OK;

} /* GetObjectType */	

int MatchArg( arg, table, flags )
	register char *arg;
	register char *table[];
	int flags;
{

	register int i;

	/* Loop thru each entry in the table */
	for ( i = 0; table[ i ]; i++) {

		if ( Match( arg, table[ i ], flags ) ) return i;
	}

	/* Here on mismatch */
	return -1;

} /* end MatchArg */

int Match( a, b, flags )
	register char *a;
	register char *b;
	int flags;
{

	register char c, d;

	/* Loop until end of shorter string */
	for ( ; *a && *b; a++, b++ ) {

		/* Use temps to avoid trashing passed parms */
		c = *a;
		d = *b;

		/* Perform case insensitivity if needed */
		if ( flags & IGNORECASE ) {
			if ( isupper( c ) )
				c = tolower( c );
			if ( isupper( d ) )
				d = tolower( d );
		}

		/* Compare strings */
		if ( c != d ) return FALSE;
	}

	/* Here on match - check for extact match */
	return (flags & EXACTMATCH? *a == *b : TRUE);

} /* end Match */

int SysError( msg )

	char *msg;

{

	fprintf( stderr, "%s: %s (%d", progname, msg, errno);
	if (errno > 0 && errno < sys_nerr)
		fprintf( stderr, ";%s)\n", sys_errlist[ errno ]);
	else
		fprintf( stderr, ")\n");
	return -1;
}

int ErrChk( stream, err )

	FILE *stream;
	int err;

{
	char buf[ 80 ];
	char buf1[ 80 ];
	int n;
	char *stream_name;

	switch ( n = ((stream - stdin) / sizeof( FILE )) ) {
	case 0:
		stream_name = "stdin";
		break;
	case 1:
		stream_name = "stdout";
		break;
	case 2:
		stream_name = "stderr";
		break;
	default:
		(void)sprintf( buf, "Stream %d", n);
		break;
	}

	(void)sprintf( buf1, "stdio error on %s", stream_name );

	if ( err == 0 ) {
		if ( ferror( stream ) || errno )  {

			(void)SysError( buf1 );
			return -errno;
		}

		if ( feof( stream ) ) return -(sys_nerr + 1);

		fprintf( stderr,
			 "%s: %s - unexpected stdio 0 return\n",
			 progname,
			 buf1 );
		return -(sys_nerr + 2);
	}

	fprintf( stderr, "%s: %s - length mismatch", progname, buf1 );
	return -sys_nerr;
}

