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
#ident	"$Header: f2csubs.c,v 1.7.2.2 90/05/09 16:49:00 wje Exp $"

#include "stdio.h"
#include "mkentry.h"
#include "tokens.h"
#ifdef YYDEBUG
int yydebug;
#endif

static struct loc_s zeroloc;

#ifdef SYSV
#define findchar(a,b) strchr(a,b)
char *strchr();
#else
#define findchar(a,b) index(a,b)
char *index();
#endif

#include "macros.h"
#include <ctype.h>
char *progname = "mkf2c";

int extendfloats = 1;
int lowercase_fortran = 1;
#ifdef mips
int default_signed = 0;
#else
int default_signed = 1;
#endif

int f77_extname_max = 6;

#ifdef NOTDEF
struct loc_s {
	/* offset from sp of the parameter's arg location */
	int stack_offset;
	/* parameter's register location, if passed in register */
	int reg;
	/* len_offset is for char strings only */
	int len_offset; 
	/* string pointer copy is for char strings (value parameters 
	   (char *)) only.  A copy of the pointer to the space
	   calloc'd is saved so that it can be free'd
	*/
	int strptrcopy_offset;
} ;

typedef struct {
	char name[TGTLANG_IDMAX+2];
	int class,type;
	unsigned flags;
	struct loc_s source;
	struct loc_s dest;
	} arg;
#endif

main(argc,argv) int argc; char **argv;
{

	char *sp,*cptr,*ctime();
	long ltime;
	int iarg = 1;

	while (iarg < argc) {
		
		if (argv[iarg][0] == '-') {

			switch (argv[iarg][1]) {

			case 'f': /* dont extend floats to doubles */
				  extendfloats = 0;
				  break;
			
			case 'U':
				 lowercase_fortran = 0;
				 break;

			case 'o':
				 if (iarg < (argc-1)) {
					iarg++;
				 	if (opfnm)
					    error(
			"-o target of %s supercedes previous target of %s\n",
						argv[iarg],opfnm);
					opfnm = argv[iarg];
				 }
				 else
					error("-o has no argument\n");
				 break;

			case 'l':
				 f77_extname_max = SRCLANG_IDMAX;
				 break;

			case 's':
				 if (!strcmp(&argv[iarg][1],"signed")) {
					default_signed = 1;
					break;
				 }
			case 'u':
				 if (!strcmp(&argv[iarg][1],"unsigned")){
					default_signed = 0;
					break;
				 }
				/* else give 'unknown switch' message */

			default:
				error("unrecognized switch (%s) - ignored.\n",
				 &argv[iarg][1]);
			}
		}
		else {
			if (!ipfnm)
				ipfnm = argv[iarg];
			else if (!opfnm)
				opfnm = argv[iarg];
			else
				error("additional filename (%s) ignored.",
					argv[iarg]);
		}
		
		iarg++;
	}

	if (opfnm) {
		if (freopen(opfnm,"w",stdout) == NULL) {
			fatal("cant open output file %s\n",opfnm);
		}
		COMMENT2("output file is ",opfnm);
		ltime = time(0);
		cptr = ctime(&ltime);
		if ((sp = findchar(cptr,'\n'))!=NULL) *sp = '\0';
		COMMENT2("generated on ",cptr);
	}
	else {
		COMMENT("output file is <stdout>");
		opfnm=(char *)0;
	}

	if (ipfnm) {
		if (freopen(ipfnm,"r",stdin) == NULL) {
			fatal("cant open input file %s\n",ipfnm);
		}
		COMMENT2("input file is ",ipfnm);
	}
	else {
		COMMENT("input file is <stdin>");
		/* scr1250 - ipfnm must be null string 
		   rather than null pointer */
		ipfnm = "";
	}


#ifdef YYDEBUG
	yydebug = 1;
#endif
	doparse();
	finishup();
	exit(0);
}

unhome(curarg)
arg *curarg;
{
	/* if the argument is to be passed in a register, load it
	   from its home location.
	*/
	struct loc_s *locp;
	locp = &curarg->source.arg;
	if (locp->reg) {
		/* otherwise, the home location is the
		   only one. 
		*/
		UNHOME(locp);
		locp = &curarg->source.string_len;
#ifdef NOTDEF
		if ((loc.offset) && (loc.reg)) {
			UNHOME(loc.reg, loc.offset);
		}
#endif
		if (curarg->type == CHARARRAY) 
			UNHOME(locp);
	}
		
}

home(curarg)
arg *curarg;
{
	/* store the given argument in its home location. */
	struct loc_s *locp;
	locp = &curarg->source.arg;
	if (locp->reg) {
		/* otherwise, the arg is already in its home
		   location. 
		*/
		HOME(locp);
		locp = &curarg->source.string_len;
#ifdef NOTDEF
		if ((loc.offset) && (loc.reg)) {
			HOME(loc.reg, loc.offset);
		}
#endif
		if ((curarg->type == CHARARRAY)||(curarg->type == STRING)) 
			HOME(locp);
	}
}

load(loc)
struct loc_s loc;
{

	/* load the passed argument into general register 2 */
	if ((loc.reg)&&(!(loc.flags & L_ARGHOMED)))
		MOVE_REGARG(2,loc.reg)
	else
		LOAD_STKLONG(2,loc.offset);
}

#define DEST 1
#define STRINGLEN 2
#define STRPTRCOPY 3

store(curarg,srcreg,part)
arg *curarg;
int part;
{
	struct loc_s loc;
	int doubleword = 0;
	
	if (part == DEST) {
		doubleword = (curarg->type == LFLOAT);
		loc = curarg->dest.arg;
	}
	else if (part = STRINGLEN) 
		loc = curarg->dest.string_len;
	else {
		loc.reg = 0;
		loc.offset = curarg->dest.strptrcopy_offset;
	}

	/* store the data in the indicated source register on the stack at
	   the address indicated for the current argument's destination.
	*/

	if (loc.reg > 0) {
		MOVE_REGARG(loc.reg,srcreg);
		if (doubleword) MOVE_REGARG(loc.reg+1,srcreg+1);
	}
	else if (loc.reg < 0) {
		if (doubleword) {
			MOVE_REG_TO_FP_D((srcreg),(loc.reg));
		}
		else {
			MOVE_REG_TO_FP((srcreg),(loc.reg));
		}
	}
	if ((!loc.reg) || (part == STRINGLEN)) {
		STORE_STKLONG(2,loc.offset);
		if (doubleword) STORE_STKLONG(3,(loc.offset + 4));
	}
}

store_fp(curarg,srcreg)
arg *curarg;
{
	struct loc_s loc;
	char szchar;
	
	szchar = (curarg->type == LFLOAT)?'d':'s';
	loc = curarg->dest.arg;

	/* 
	   store the data in the indicated fp source register stack at
	   the address indicated for the current argument's destination.
	*/

	if (loc.reg > 0) {
		/* store from a floating-point register to a general register */
#ifdef NOTDEF
		printf("\ts.%c\t$f%d, $%d\n", szchar,
			(-srcreg),loc.reg);
#endif
		if (curarg->type == LFLOAT) {
			MOVE_REG_FROM_FP_D((loc.reg),(srcreg));
		}
		else {
			MOVE_REG_FROM_FP((loc.reg),(srcreg));
		}
	}
	else if (loc.reg < 0) {
		printf("\tmov.%c\t$f%d, $f%d\n", szchar,
			(-srcreg), (-loc.reg));
	}
	else {
		printf("\ts.%c\t$f%d, %d($sp)\n", szchar,
			(-srcreg), loc.offset);
	}
}


extend_float(curarg)
arg *curarg;
{
	int destreg;
	struct loc_s destloc ;

	/* the given arg is a pointer to a float which
	   resides in register $2.  It is to be dereferenced,
	   extended to a double and stored in the destination.
	*/
	destloc = curarg->dest.arg;

	COMMENT3("parameter '",curarg->name, "' - extended float value");

	/* a pointer to the unextended floating-point value is in $2 */
	if ((destreg = destloc.reg) >= 0) {
		/* cant use the target floating-point register */
		destreg = (-4);
	}
	/* dereference the float, and leave it in destreg */
	LOAD_IND_FLOAT(destreg,2);
	/* extend the float */
	if ((curarg->type == FLOAT) && (extendfloats)) {
		EXTEND_FLOAT(destreg,destreg);
		curarg->type = LFLOAT;
	}
	/* if this is not the final location, move it */
	if (destreg == (-4))
		store_fp(curarg,destreg);
}

dereference(curarg)
arg *curarg;
{

	int type = curarg->type;
	int class = curarg->class;
	int stored = 0;

	switch (type) { 
	

	    case SHORT: /* */
			if (class == UNSIGNED) {
				COMMENT3("parameter '",curarg->name,
					    "' - unsigned short value");
				LOAD_INDU_STKVAL(2,0,'h');
			} 
			else  {
				COMMENT3("parameter '",curarg->name,
					    "' - signed short value");
				LOAD_IND_STKVAL(2,0,'h');
			}
			break;

	    case CHAR:
			if (class == UNSIGNED) {
				COMMENT3("parameter '",curarg->name,
					    "' - unsigned char value");
				LOAD_INDU_STKVAL(2,0,'b');
			} 
			else  {
				COMMENT3("parameter '",curarg->name,
					    "' - signed char value");
				LOAD_IND_STKVAL(2,0,'b');
			}

			break;

	    case LFLOAT:
			COMMENT3("parameter '",curarg->name,
					    "' - double value");
			if (curarg->dest.arg.reg < 0) {
				LOAD_IND_DOUBLE(curarg->dest.arg.reg,2);
				stored++;
			}
			else {
				LOAD_IND_STKVAL(3,4,'w');
				LOAD_IND_STKVAL(2,0,'w');
			}
			break;

	    
	    default:	/* case for INT, LONG, FLOAT */
			if (type & FLOAT) {
			    COMMENT3("parameter '",curarg->name,
					  "' - unextended float value");
			    if (curarg->dest.arg.reg < 0) {
				LOAD_IND_FLOAT(curarg->dest.arg.reg,2);
				stored++;
				break;
			    }
			}
			else
				COMMENT3("parameter '",curarg->name,
					  "' - 32-bit value");

			LOAD_IND_STKVAL(2,0,'w');
			break;

	}
	if (!stored)
		store(curarg,2,DEST);
}

#define STATIC  0x800
#define LONG	0x400	 /* this is assigned to yylval */
#define SIGNED  0x200
#define UNSIGNED 0x100
#define SIGN_SPECIFIED(c) ( (c) & (SIGNED|UNSIGNED))


/* types */
#define INT	 0
#define CHAR	0x1000
#define SHORT	0x2000
#define FLOAT	0x4000
#define BYTE	0x8000
#define LFLOAT  (FLOAT|LONG)
/* arbitrary pointer - only restriction is NOT to CHAR */
#define PTR	0x10000
#define ARRAY	0x20000
#define PTRPARM (PTR|ARRAY)

#define STRING	(PTR|CHAR)
#define CHARARRAY (ARRAY|CHAR)
dumparg(i,curarg)
arg *curarg;
{
	char *str;
	printf(" # arg #%d info:  parameter ' %s ', ",i,curarg->name);
	if (curarg->class & STATIC) printf("static ");
	if (curarg->class & SIGNED) printf("signed ");
	if (curarg->class & UNSIGNED) printf("unsigned ");
	switch (curarg->type) {
		case INT:	
			str = "32-bit value";
			break;
		case CHAR:	
			str = "char value";
			break;
		case SHORT:	
			str = "short value";
			break;
		case FLOAT:	
			str = "float value";
			break;
		case LFLOAT:	
			str = "double value";
			break;
		case STRING:	
			str = "character string copy";
			break;
		case CHARARRAY:	
			str = "character array";
			break;
		default:
			if (curarg->type & PTRPARM)
				str = "simple pointer";
			else
				str = "????";
	}
	printf("%s",str);
	printf("\n #\tsource = $%d/%d(strlen = $%d/%d). dest = $%d/%d(strlen = $%d/%d)\n",
		curarg->source.arg.reg, curarg->source.arg.offset,
		curarg->source.string_len.reg, curarg->source.string_len.offset,
		curarg->dest.arg.reg, curarg->dest.arg.offset,
		curarg->dest.string_len.reg, curarg->dest.string_len.offset);
	printf(" #\tmalloc_off = %d\n #\n",curarg->dest.strptrcopy_offset); 
}

	
process_string(curarg)
arg *curarg;
{
	/* the current argument is a character string, which must
	   be copied and null-terminated.  It is assumed that all
	   arguments are in their home locations, as this routine
	   will re-use the argument registers.
	*/
	
	struct op_s *srcop = &curarg->source,*destop = &curarg->dest;

	/* the hard case - character strings */
	COMMENT3("parameter '",curarg->name,
		"' - character string copy");

	/* call calloc to get space for the string.
		calloc(1,len)
	*/
	LOAD_REGVAL(4,1);

	/* get the length of the string */
	LOAD_STKLONG(5,srcop->string_len.offset);
	/* add one to it */
	ADD_REGVAL(5,1);
	CALL("calloc");
	/* the result (in register 2) is the pointer to the
	   new string.  It is now to be saved in the mallocptr
	   slot, and in 
	   the first argument register (for the call to strncpy) 
	   It will be stored over the old string pointer
	   in the home location of the argument, after the old
	   string pointer is loaded.
	*/
	MOVE_REGARG(4,2);
	/* get the original string pointer */
	LOAD_STKLONG(5,srcop->arg.offset);
	/* and store the new string pointer over it. */
	STORE_STKLONG(2,srcop->arg.offset);
	/* and in our local copy */
	STORE_STKLONG(2,destop->strptrcopy_offset);
	/* get the length of the string */
	LOAD_STKLONG(6,srcop->string_len.offset);
	/* and call strncpy */
	CALL("strncpy");
}

#define ISODD(reg) (reg & 1)

eftn()
{
	int arg_offset,space_used;
	arg *curarg;
	int i,class;
	int type;
	int frame_growth = 0;
	int nmallocptrs = 0;
	struct op_s *srcop,*destop;

#ifdef DEBUG
	fprintf(stderr,"eftn called\n");
	fprintf(stderr,"nargs = %d\n",nargs);
#endif
	if ((ftn.class & ~CLASS) == STATIC) return;

	ftn.nargs = nargs;
	
	/* generate the entry code */
	TEXTCSEG;
	GLOBL(ftn.tgtentry);
	GLOBL(ftn.srcentry);
	COMMENT3(ftn.srcentry,"(FORTRAN entry) for calling C routine",ftn.tgtentry);
	ENT(ftn.srcentry);
	LABEL(ftn.srcentry);

	/* eftn - the real work.  All of the parameters have been
	   declared and typed. We have to do the following work:

	   STEP ONE:

	   Beginning with the first argument and proceeding to the
	   last, the arg location and the register location (if any)
	   of the arguments is determined and placed in the arg structure.
	   The arg location of the first argument is 0($sp).
	*/

	if (nargs) {
	    int usefregs = 0;
#ifdef NOTDEF
	    int nstringlens = 0;
#endif
	    int source_argoffset = 0;
	    int dest_argoffset = 0;
	    int source_argregs_used = 0;
	    int dest_argregs_used = 0;
	    int first_dest_strlen;
	    int fudge_sngl_fparg0=0;
	    int nwords;

	    if (arglist[0].type & FLOAT) 
		usefregs++;

	    for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) {
		/* determine the argument locations */

		curarg->flags = 0;
		curarg->source.arg.reg = curarg->dest.arg.reg = 0;
		curarg->relstring = (-1);

		curarg->source.arg.offset = source_argoffset;
		source_argoffset += 4;

		/* the source is ALWAYS a pointer. */
		if (source_argregs_used < NARGREGS) {
			curarg->source.arg.reg = 
				GENREG(FIRST_ARGGREG + source_argregs_used);
			source_argregs_used++;
		}
		else
			curarg->source.arg.flags |= L_ARGHOMED;

		/* if the destination is a pointer, just pass it. */
		if ((type = curarg->type) & PTRPARM) {
			usefregs = 0;
			if (dest_argregs_used < NARGREGS) {
				curarg->dest.arg.reg = 
				    GENREG(FIRST_ARGGREG + dest_argregs_used);
				dest_argregs_used++;
			}
			curarg->dest.arg.offset = dest_argoffset;
			dest_argoffset += 4;

#ifdef NOTDEF
			if ((type == STRING)||(type == CHARARRAY))
				nstringlens++;
#endif

			if (type == STRING) 
				curarg->relstring = nmallocptrs++;
		} 
		else {
		    
		    	/* the destination type is NOT a pointer.  We must
		       	dereference it, and possibly move it around. 
		    	*/
		    	curarg->flags |= DEREFERENCE;
			if (i>1)
				usefregs = 0;

			if ((type == FLOAT)&&(extendfloats))
				type = LFLOAT;

			nwords=1;

			switch (type ) {

			    case LFLOAT:
				nwords=2;
				if (ISODD(
			GENREG(dest_argregs_used + FIRST_ARGGREG))) {
					dest_argregs_used++;
					dest_argoffset += 4;
				}
				fudge_sngl_fparg0 = 0;

			    case FLOAT:
				if (dest_argregs_used<(NARGREGS-1)) 
					    curarg->dest.arg.reg = 
						usefregs?
	FLTREG((dest_argregs_used+fudge_sngl_fparg0) + FIRST_ARGFREG):
			GENREG(dest_argregs_used + FIRST_ARGGREG); 

				if ((!i)&&(type == FLOAT))
					fudge_sngl_fparg0 = 1;

				break;

			    default: 
				usefregs = 0;
				/* any INT type */
				if (dest_argregs_used < NARGREGS) {
					curarg->dest.arg.reg = 
			GENREG(dest_argregs_used + FIRST_ARGGREG); 
				}
			    }
			    curarg->dest.arg.offset = 
					dest_argoffset;
			    dest_argregs_used+= nwords;
			    dest_argoffset += (nwords<<2);
		}
	    }
	    first_dest_strlen = dest_argoffset;
	    /* 
		STEP TWO:

		March down the arglist, assigning the locations of string
		lengths and their copy pointers (for FREE'ing the space).

	    */
	    for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) {
		/* determine the argument locations */

		type = curarg->type;

		if ((type == CHAR) || 
		    (type == STRING) || (type == CHARARRAY)) {
			if (source_argregs_used < NARGREGS) {
				curarg->source.string_len.reg = 
				    GENREG(FIRST_ARGGREG + source_argregs_used);
				source_argregs_used++;
			}
			else
				curarg->source.string_len.flags |= L_ARGHOMED;

			curarg->source.string_len.offset = source_argoffset;
			source_argoffset += 4;

			if (type == CHARARRAY) {
			    if (dest_argregs_used < NARGREGS) {
				curarg->dest.string_len.reg = 
				    GENREG(dest_argregs_used + FIRST_ARGGREG); 
				dest_argregs_used++;
			    }
			    curarg->dest.string_len.offset = dest_argoffset;
			    dest_argoffset += 4;
			}

#ifdef NOTDEF
			if (type == STRING) {
				curarg->dest.strptrcopy_offset =
				 -( 4 + (nstringlens * 4) - (curarg->relstring *4));
			}
#endif
		}
	    }

	    frame_growth = dest_argoffset - source_argoffset;

	    if (frame_growth < 0) frame_growth = 0;

	    if (nmallocptrs||frame_growth) {
	        /* add space in the dest frame for the mallocpointers */
	    	frame_growth += (nmallocptrs * 4) ;
		/* add in the size of a new stack frame.  This
		   is max(20,argsize)
		*/
		if (source_argoffset < 20)
			source_argoffset = 20;
		frame_growth += source_argoffset;
	    }
	
	    /* 
		STEP THREE:
		
		if the new framesize is larger than the old framesize,
		recompute the source locations and emit the instruction
		to extend the stack.

	    */

	    if (frame_growth) {
		
		/* allocate space for the link register */
		frame_growth += 4;

		/* make the stack doubleword aligned */
		if (frame_growth & 4) frame_growth += 4;

	        for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) {
		    /* add the frame growth to the source argument locations */
		    curarg->source.arg.offset += frame_growth;
		    /*curarg->dest.arg.offset += frame_growth; */
		    if ((curarg->type == CHARARRAY)||(curarg->type == STRING)) {
			    curarg->source.string_len.offset += frame_growth;
		    	    /* curarg->dest.string_len.offset += frame_growth;*/
			    if (curarg->type == STRING)
				curarg->dest.strptrcopy_offset =
					source_argoffset + 4*curarg->relstring;
		    }

	        }

		UPDATE_SP((-frame_growth));
		EMIT_LINK_AND_MASK(frame_growth-4);

	    }
	}
	/* and emit the frame instruction for the debugger */
	FRAME(frame_growth);

	/* emit the argument descriptions */
	for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) 
		dumparg(i,curarg);

	/* if we have to do any string copying, home the
	   args 
	*/
	if (nmallocptrs) {
		for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) 
			home(curarg);
		for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) 
			if (curarg->type == STRING)
				process_string(curarg);
		for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) 
			unhome(curarg);
	}

	/*
	    STEP FOUR: 	Move the arguments to their final locations, possibly
		 	dereferencing them.

		1. if the argument in C is a value, the value must
		   be loaded (from its address) and pushed.  If the
		   value is NOT 32 bits, it must be extended.

		2. if the argument in C is an address and NOT a character
		   string, the address on the stack is copied.

		3. if the argument in C is a character string, we must:
			
			a. take the length of the Fortran string off
			   the stack.

			b. use the length to calloc space for a copy of 
			   the string.

			c. copy the string to the calloc'd area.

			d. use the newly-allocated string as the argument.

			e. on return, this string must be free'd.

	*/

	/* space_used is the amount of stack space we have used in
	   the C stack frame */
	space_used = 0;

	curarg = &arglist[nargs-1];
	for (i=(nargs-1); i>=0; (i--),curarg--) {
/*	for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) { */

	    class = curarg->class;
	    type = curarg->type;
	    srcop = &curarg->source;
	    destop = &curarg->dest;

	    /* if the source location is the same as the destination
	       location and the parameter is NOT to be dereferenced,
	       there is nothing to do
	    */
	    dumparg(i,curarg);
	    if (/*(type != STRING) && */
		 ((curarg->flags & DEREFERENCE) ||
		  (!(srcop->arg.flags & L_ARGHOMED)) ||
		  ((!srcop->arg.reg)&&(srcop->arg.offset!=destop->arg.offset))||
		  ((srcop->arg.reg) && (srcop->arg.reg != destop->arg.reg)))){

		/* load the source. */
		load(srcop->arg);

		/* store the data in the destination, possibly 
		   dereferencing it. */
		if ((curarg->type == FLOAT)&&(extendfloats)&&
		    (curarg->flags & DEREFERENCE)) 
			extend_float(curarg);
		else if (curarg->flags & DEREFERENCE) 
		    	dereference(curarg);
		else
			store(curarg,2,DEST);

	    }
	    if ((type == CHARARRAY) && 
	      	 ( (!(srcop->string_len.flags & L_ARGHOMED)) ||
		  ((!srcop->string_len.reg)&&
		     (srcop->string_len.offset!=destop->string_len.offset))||
		  ((srcop->string_len.reg) && 
		     (srcop->string_len.reg != destop->string_len.reg)))){
		/* see if the length has to be moved */
		load(srcop->string_len);
		store(curarg,2,STRINGLEN);

	    }
	}

	if (frame_growth) {

	    CALL(ftn.tgtentry);
	    if (nmallocptrs) {
		/* run the arg list, freeing the space malloc'd for
		   string copies.
		*/
		for (i=0,curarg= arglist; i<nargs ; (i++),curarg++) 
			if (curarg->type == STRING)  {
				/* free the string copy */
				LOAD_STKLONG(4,curarg->dest.strptrcopy_offset);
				CALL("free");
			}
	    }
	    /* restore the link register */
	    LOAD_STKLONG(31,(frame_growth-4));
	    UPDATE_SP(frame_growth);
	    JUMPREG(31);
	}
	else
	    JUMPSYM(ftn.tgtentry);

	END(ftn.srcentry);



}




finishup()
{
#ifdef DEBUG
	fprintf(stderr,"finish up called\n");
#endif

	if (stringsused) {
		GLOBL("_strncpy");
		GLOBL("_calloc");
	}
}





procarg()
{
	arg *curarg;
#ifdef DEBUG
	fprintf(stderr,"procarg called.  ");
	fprintf(stderr,"token is %s\n",lastname);
#endif
	/* procarg - the name of an argument has been spotted in 
	   a parameter list.  fill in the correct slot in arglist[]
	   Make the argument INT.
	*/
	if ((ftn.class & ~CLASS) == STATIC) return;

	if (nargs > MAXPARMS) 
		fatal(
"maximum number of parameters exceeded\nat parameter %s in function %s\n",
			lastname,ftn.tgtentry);

	curarg = &arglist[nargs];

	/* copy the argument string to the slot */
	strcpy(curarg->name,lastname);
	curarg->class = 0;
	curarg->type = INT;
	curarg->source.arg = zeroloc;
	curarg->source.string_len = zeroloc;
	curarg->dest.arg = zeroloc;
	curarg->dest.string_len = zeroloc;
}






matcharg()
{
	int i;
	arg *curarg;
#ifdef DEBUG
	fprintf(stderr,"matcharg called.  curclass = 0x%x, curtype= 0x%x\n",
			 curclass,curtype);
	fprintf(stderr,"token is %s\n",lastname);
#endif
	if ((ftn.class & ~CLASS) == STATIC) return;
	/* matcharg - the declaration of an argument has been
	   spotted.  Match it with an argument in the parameter list
	   and fill in the class and type. */

	for (i=0,curarg=arglist;i<nargs;i++,curarg++) {
		if (!strcmp(lastname,curarg->name)) {
			/* match found */
#ifdef DEBUG
	fprintf(stderr,"match found arg #%d\n",i);
#endif
			if ((curtype & PTR)&&(curtype != STRING)) 
				curarg->type = PTR;
			else if ((curtype == FLOAT) && (curclass == LONG)){
				curarg->type = LFLOAT;
				curclass = 0;
			}
			else if (curtype == SU) {
					warning(
			"struct/union parameter assumed indirect. - simple pointer passed\n");
				curarg->type = PTR;
			}
			else
				curarg->type = curtype;
			if (curtype == STRING) {
				newstackframe++;
				localspace -= 4;
				curarg->dest.strptrcopy_offset = localspace;
			}
			else if (curtype == LFLOAT) {
				newstackframe++;
			}
			else if ((curtype == FLOAT) && (extendfloats)) {
				newstackframe++;
			}
			else if (curtype == CHAR) {
				if (!(SIGN_SPECIFIED(curclass))) 
					curclass |= 
					    (default_signed)?SIGNED:UNSIGNED;
			}
			curarg->class = curclass;
			return;
		}
	}
	fatal(
"parameter %s in type declaration of function %s was not in parameter list.",
		lastname,ftn.tgtentry);
	yyerror();
 
	
	
}


bftn()
{
	char *cptr,*fnameptr;
	int len;
#ifdef DEBUG
	fprintf(stderr,"bftn called with function %s\n",lastname);
	fprintf(stderr,"class = %d, type = %d\n",curclass,curtype);
#endif
	/* the function name, type and class has been read. 
	   we have to do the following things:

		*  assemble the entrypoint names in the ftn structure.
		*  fill in the type and class data.
		*  initialize some variables.

	*/

	/* form the C entry name */
	if ((len = strlen(lastname)) > (TGTLANG_IDMAX)) {
		error("%d too many characters in function name (%s).\n\tTruncated to maximum of %d",len-TGTLANG_IDMAX,lastname,TGTLANG_IDMAX);
		lastname[TGTLANG_IDMAX] = 0;
	}
	strcpy(ftn.tgtentry,lastname);
	/* and the Fortran entry */
	for (cptr = lastname, fnameptr = ftn.srcentry ; 
	     (*cptr && fnameptr<&ftn.srcentry[f77_extname_max]) ; cptr++) {

		if (isalnum(*cptr)) {
		    if ((isupper(*cptr))&&(lowercase_fortran)) {
			*fnameptr++ = (tolower(*cptr));
		    }
		    else *fnameptr++ = *cptr;
		}
		else warning(
		"removing illegal character %c (\\%3.3o) in FORTRAN entry for function %s\n",
			*cptr,*cptr,lastname);
	}
	*fnameptr++ = '_';
	*fnameptr++ = 0;

	ftn.type = curtype; ftn.class = curclass;
	if ((curclass & ~CLASS)== STATIC)
		warning(
	"static function (%s) ignored.\n",
		ftn.srcentry);
	if (curtype == CHAR)
		warning(
	"function (%s) type of CHAR corresponds to FORTRAN type of INT*1\n",
		ftn.tgtentry);

	localspace = 0;


}

