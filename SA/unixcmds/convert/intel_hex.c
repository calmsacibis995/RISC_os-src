#ident "$Header: intel_hex.c,v 1.4.4.1 90/07/18 16:43:01 huang Exp $"
/* $Header: intel_hex.c,v 1.4.4.1 90/07/18 16:43:01 huang Exp $ */
/* $Log:	intel_hex.c,v $
 * Revision 1.4.4.1  90/07/18  16:43:01  huang
 * Branch GENESIS_BETA1 off of trunk.
 * 
 * Revision 1.4  90/05/03  11:08:13  lian
 * Changed record_address variable from static to extern.
 * 
 * Revision 1.3  90/03/01  12:02:41  chungc
 * added -i option support.
 * 
 * Revision 1.2  89/12/06  13:42:51  chungc
 * generates extended address intelhex format to handle eproms 
 * bigger than 64K bytes.
 * 
 * Revision 1.1  87/08/18  16:52:55  mdove
 * Initial revision
 *  */

#include "convert.h"
#include "intel_hex.h"

/* 
 * Record types
 */
#define DATA_RECORD 0x00
#define EOF_RECORD  0x01
#define EADR_RECORD 0x02

#define HEXIFY1( source, destination ) \
	*destination++ = HEX( ((source >> 4) & 0xf)); \
	*destination++ = HEX( source & 0xf); \
	checksum += source & 0xff;
static char *hex_digits = "0123456789ABCDEF";
#define HEX( x ) ( hex_digits[ (int)x ] )



/* static address record_address; */
extern address record_address;

static char *data;
static address offset_address;
static unsigned int checksum;

/* Forward and external declarations */
extern char *calloc();
unsigned int IntelhexChecksum();

int IntelhexInitialize( record_size )
	
	int record_size;

{
	unsigned int length;

	length = 12 + (record_size * 2);

	return !(data = calloc( length, 1 ));
}

int StepConvert( buffer, length )

	char *buffer;
	int length;

{
	char *cdp = data;
	
	checksum = 0;
	*cdp++ = ':';
	HEXIFY1( (unsigned int)length, cdp );
	cdp += Hexify2( (unsigned int)record_address, cdp );
	HEXIFY1( DATA_RECORD, cdp );
	cdp += Hexify( buffer, cdp, length );
	HEXIFY1( -checksum, cdp );

	record_address += length / format->blocking_factor;
	cdp = 0;

	return OK;
}

int IntelhexConvert( buffer, length )

	char *buffer;
	int length;

{
	char *cdp = data;

	/* put out extended address record at the beginning and when necessary */
	if (!record_address || (record_address+length > 0x10000))
	{
	    offset_address += record_address & 0xfffffff0;
	    Intelhex_eadr_record(offset_address);
	    record_address -= record_address & 0xfffffff0;
	}

	/* put out a data record */
	checksum = 0;
	*cdp++ = ':';
	HEXIFY1( (unsigned int)length, cdp );
	cdp += Hexify2( (unsigned int)record_address, cdp );
	HEXIFY1( DATA_RECORD, cdp );
	cdp += Hexify( buffer, cdp, length );
	HEXIFY1( -checksum, cdp );

	record_address += length;
	*cdp = 0;

	return OK;
}
int IntelhexWrite() {

	puts( data );

	return 0;
}

int IntelhexTerminate(){
	char *cdp = data;

	checksum = 0;

	*cdp++ = ':';
	HEXIFY1( 0, cdp );
	cdp += Hexify2( (unsigned int)start_address, cdp );
	HEXIFY1( EOF_RECORD, cdp );
	HEXIFY1( -checksum, cdp );

	*cdp = 0;

	return IntelhexWrite();
}


static int Hexify2( source, destination )

	unsigned short source;
	char *destination;

{


	*destination++ = HEX( ((source >> 12) & 0xf));
	*destination++ = HEX( ((source >> 8) & 0xf));
	checksum += (source >> 8) & 0xff;
	*destination++ = HEX( ((source >> 4) & 0xf));
	*destination++ = HEX( source & 0xf);
	checksum += source & 0xff;

	return 4;

}

static int Hexify1( source, destination )

	unsigned char source;
	char *destination;

{


	*destination++ = HEX( ((source >> 4) & 0xf));
	*destination++ = HEX( source & 0xf);
	checksum += source & 0xff;

	return 2;

}

static int Hexify( source, destination, length)

	register char *source;
	int length;
	register char *destination;

{

	register char src;
	register int i = length;
	register unsigned int cksum = 0;
	register int index;
	register int increment;
	extern int byteswap;

	/* For split converts chose which byte of the instruction */
	index = byte_number < 0 ? 0 : byte_number;
	increment = byte_number < 0 ? 1 : 4;

	while ( i-- > 0 ) {

		/* Get the byte to be processed */
		src = source[ index ];

		if (byteswap) {
			register char s;

			s = source[index + increment];
			/* Convert it to hex ascii */
			*destination++ = HEX( (s >> 4) & 0xf);
			*destination++ = HEX( s & 0xf);
			cksum += (unsigned)s;
		}

		/* Convert it to hex ascii */
		*destination++ = HEX( (src >> 4) & 0xf);
		*destination++ = HEX( src & 0xf);
		cksum += (unsigned)src;

		/* Bump to next next byte to be processed */
		source += increment;

		if (byteswap) {
			/* Bump to next next byte to be processed */
			source += increment;
			i--;
		}	
	}

	checksum += cksum;
	return length * 2;

}


static int Intelhex_eadr_record(offset)
address offset;
{
    int length;
    char *cdp = data;
    address zero_address;

	length = 2;
	zero_address = 0;
	offset = offset >> 4;

	/* put out an extended address record */
	checksum = 0;
	*cdp++ = ':';
	HEXIFY1( (unsigned int)length, cdp );
	cdp += Hexify2( (unsigned int)zero_address, cdp );
	HEXIFY1( EADR_RECORD, cdp );
	cdp += Hexify2( (unsigned int)offset, cdp );
	HEXIFY1( -checksum, cdp );
	*cdp = 0;
	puts(data);

	return OK;
}

