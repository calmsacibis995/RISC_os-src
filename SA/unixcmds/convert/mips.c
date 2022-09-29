#ident "$Header: mips.c,v 1.2.4.2 90/07/26 15:55:57 huang Exp $"
/* $Header: mips.c,v 1.2.4.2 90/07/26 15:55:57 huang Exp $ */
/* $Log:	mips.c,v $
 * Revision 1.2.4.2  90/07/26  15:55:57  huang
 * copy from RID 1.3
 * 
 * Revision 1.3  90/07/26  14:14:58  chungc
 * puts out .lit8 and .lit4 sections.  also fixed incorrect s3rec address
 * in case of non-contiguous loadable sections.
 * 
 * Revision 1.2  90/05/03  11:15:03  lian
 * Added change for multiple file support. Print warning if
 * object sections not in asceding address order.
 * 
 * Revision 1.1  87/08/18  16:52:58  mdove
 * Initial revision
 *  */

/******************************************************************************* *
 *	This module opens, reads, and closes MIPS coff object Files	       *
 *
 *******************************************************************************
 */
#define S3_DATA_RECORD 0x03
#define mips 1
#define LANGUAGE_C 1
#include <stdio.h>
#include <strings.h>
#include "filehdr.h"
#include "aouthdr.h"
#include "scnhdr.h"
#include "convert.h"
/* #define DEBUG	*/

/*
 *  Object File Section Externs
 */
extern address load_address;		/* start address of current section */
extern address section_end_address;	/* end address of current section */

extern char *objname;			/* object file name */

/*
 *  Mips Object File data structures
 */
static struct filehdr file_header;

static struct aouthdr optional_header;

static struct scnhdr section;

/*
 *  Module specific data
 */

static long section_headers;		/* file pointer to start of sections */
static long next_header;

struct section_type {
	long flags;
	char name[ 8 ];
};

static struct section_type section_types[] = {
	(long)STYP_TEXT, {_TEXT},
	(long)STYP_INIT, {_INIT},
	(long)STYP_RDATA, {_RDATA},
	(long)STYP_DATA, {_DATA},
	(long)STYP_LIT8, {_LIT8},
	(long)STYP_LIT4, {_LIT4},
	(long)STYP_SDATA, {_SDATA}
};
#define N_SECTION_TYPES (sizeof section_types /  \
			     sizeof(struct section_type ))


static int sti;			/* Section Table index */

static unsigned long section_size;

int MipsInitialize() {

	int error;

	/*
	 *  Read in the headers
	 */

	if ( (error = fread( (char *)&file_header,
			     sizeof( struct filehdr ), 1, stdin)) != 1 )
		return ErrChk( stdin, error );

	/* Fixup if of opposite sex */
	if ( file_header.f_magic == SMIPSEBMAGIC || 
	     file_header.f_magic == SMIPSELMAGIC )  {
		swap = TRUE;
		swap_filehdr( &file_header );
	}

	/* Read optional header if consistent */
	if (file_header.f_opthdr != sizeof(struct aouthdr) ) {
		fprintf(stderr,
		 	"Error: File and optional headers inconsistent\n" );
		return -3;
	}

	if (fread( (char *)&optional_header,
		   sizeof(struct aouthdr ), 1, stdin) != 1 )
		return -2;

	/* Fixup if of opposite sex */
	if ( swap )  swap_aouthdr( &optional_header );

	start_address = (unsigned)optional_header.entry;
#ifdef DEBUG
	printf("text_start= %x data_start= %x\n",optional_header.text_start,optional_header.data_start);
#endif DEBUG

	/*
	 * Open the first text section
	 */
	next_header = section_headers = ftell( stdin );
	sti = 0;
	return (GetNextSection());
}


/*
 *  MipsRead - Read data from a Mips/Coff Object file.
 *
 *  Inputs:  buffer - address of buffer for data
 *	     length - number of bytes to read
 *  Outputs: buffer - gets the data
 *  returns: >0 - Ok number of bytes read
 *	     =0 - End of file
 *	     <0 - Error code
 */
int MipsRead( buffer, length )

	char *buffer;
	int length;

{
	int error;

	/* Check for end of section */
	if (section_size <= 0) {

		/* Get the next section */
		error = GetNextSection();

		if (error < 0 ) return error;	/* Error? */
		if (error > 0 ) return 0;		/* Eof?   */
	}

	/* Is there enough data in the buffer to satisfy the request? */
	if ( length > section_size ) 
		length = section_size;
	section_size -= length;

	/* Read the data */
	if ( length > 0 &&
	     ((error = fread( buffer, 1, length, stdin)) != length ) )

		/* Physical end of file or read error should cause death */
		return ErrChk( stdin, error );
	

	/* Normal exit - return # of characters read */
	return length;
}
/*
 *  Mips Close - No special cleanup needed
 */
int MipsClose () {

	return OK;
}

/*
 *GetNextSection - Set up the next section of interest for reading
 *
 * Inputs:  None
 * Outputs: None
 * Returns: <0 if error occured
 *	    =0 if next section opened
 *	    >0 if no more sections of interest to open
 */
int GetNextSection() {

	register int i;
	register int error;
	extern address record_address;
	extern unsigned int data_record;

	while ( sti < N_SECTION_TYPES ) {

		/* Position to next header to be read */
		if (fseek( stdin, next_header, 0) ) return ErrChk( stdin, 0 );
		
		for ( i = file_header.f_nscns; i > 0; i-- ) {

			if ((error = fread( (char *)&section,
				    	    sizeof(struct scnhdr),
				    	    1,
				    	    stdin                  )) != 1 )
				return ErrChk( stdin, error );
			
			/* Fixup if of opposite sex */
			if ( swap )   swap_scnhdr( &section );

			/* Is this a header of a section to be loaded? */
			if ( section.s_flags & section_types[ sti ].flags &&
			    !strcmp( section.s_name, section_types[sti].name)) {

				/* Got one */
				next_header = ftell( stdin );
				if (section.s_vaddr != section.s_paddr)
					return -4;

				/* Ascending address order required for object sections */
				if ((section_end_address != -1) &&
				    (start_address != section.s_paddr) &&
				    (section_end_address > section.s_paddr)) {
					fprintf(stderr, "Warning: %s - object sections not in ascending address order.\n", objname);
				}
				load_address = section.s_paddr;
				section_size = section.s_size;
				section_end_address = section.s_paddr+section_size;
				if (data_record == S3_DATA_RECORD)
					(address)record_address = load_address;
				
#ifdef DEBUG
printf("data_record = %x\n",data_record);
printf("\nsection: %s load_address = 0x%x section_size = 0x%x\n\n",section.s_name,load_address,section_size);
#endif DEBUG
				return (fseek( stdin, section.s_scnptr, 0) ?
				     ErrChk( stdin, 0 ) : OK );
			}
		}

		sti += 1;
		next_header = section_headers;
	}

	/* Here if no section found */
	return 1;
}
	
