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
/* $Header: macros.h,v 1.5.2.2 90/05/09 16:49:14 wje Exp $ */

#define ADD_REGVAL(reg,val)	printf("\taddi\t$%d,$%d,%d\n",reg,reg,val)

#define CALL(str)	printf("\tjal\t%s\n",str) 

#define COMMENT3(str1,str2,str3)\
			printf(" #\n #\t%s %s %s\n #\n",str1,str2,str3)

#define COMMENT2(str1,str2) \
			COMMENT3(str1,str2,"")

#define COMMENT(str1)	COMMENT3(str1,"","")

#define END(name)	printf("\t.end\t%s\n",name)

#define ENT(fn)		printf("\t.ent\t%s 2\n",fn)

#define EMIT_LINK_AND_MASK(linkoff)  \
		printf("\tsw\t$31, %d($sp)\n\t.mask\t0x80000000, -4\n",linkoff)

#define EXTEND_FLOAT(destreg,srcreg) {\
			printf("\tcvt.d.s\t");\
			PRINTREG(destreg);\
			putchar(',');\
			PRINTREG(srcreg);\
			putchar('\n');\
			}


#define FRAME(framesize) printf("\t.frame\t$sp, %d, $31\n",framesize)

#define GLOBL(str)	printf("\t.globl\t%s\n",str)


#define JUMPREG(reg)	printf("\tj\t$%d\n",reg) 

#define JUMPSYM(sym)	printf("\tj\t%s\n",sym) 

#define LABEL(str)	printf("%s:\n",str)

#define LOAD_IND_FLOAT(freg,areg) \
		printf("\tl.s\t$f%d, 0($%d)\n",(-freg), areg)

#define LOAD_IND_DOUBLE(freg,areg) \
		printf("\tl.d\t$f%d, 0($%d)\n",(-freg), areg)

#define LOAD_INDU_STKVAL(reg,off,ch) \
	printf("\tl%cu\t$%d, %d($2)\t# get address from stack \n",ch,reg,off)
			
#define LOAD_IND_STKVAL(reg,off,ch) \
	printf("\tl%c\t$%d, %d($2)\t# get indirect signed value \n",ch,reg,off)

#define LOAD_REGVAL(reg,val)	printf("\tli\t$%d, %d\n",reg,val)
			
#define LOAD_STKLONG(reg,off) \
	printf("\tlw\t$%d, %d($sp)\t# get address from stack \n",reg,off)

#define UNHOME(locp) \
	if ((locp->flags & L_ARGHOMED)&&(locp->reg)) { \
		locp->flags &= ~L_ARGHOMED; \
		printf("\tlw\t$%d, %d($sp)\t# load argument from home location\n",locp->reg,locp->offset); \
	}

#define STORE_STKLONG(reg,off) \
	printf("\tsw\t$%d, %d($sp)\t# get address from stack \n",reg,off)

#define HOME(locp) \
	if ((!(locp->flags & L_ARGHOMED))&&(locp->reg)) {\
		locp->flags |= L_ARGHOMED; \
		printf("\tsw\t$%d, %d($sp)\t# store argument in home location\n",locp->reg,locp->offset); \
	}


#define PRINTREG(reg) \
	if (ISFLTREG(reg)) printf("$f%d",-reg); else printf("$%d",reg)

#define LOAD_FREG(destreg,srcoff,isdbl) \
	printf("\tl.%d\t$f%d, %d($sp)\n",(isdbl)?'d':'s',(-destreg),srcoff)

#define MOVE_REGARG(destreg,srcreg) {\
			printf("\tmove\t");\
			PRINTREG(destreg);\
			putchar(',');\
			PRINTREG(srcreg);\
			putchar('\n');\
			}

#define MOVE_REG_FROM_FP(destreg,srcreg) \
	printf("\tmfc1\t$%d, $f%d\t\t# move from fpa\n",destreg,(-srcreg))

#define MOVE_REG_FROM_FP_D(destreg,srcreg) \
	printf("\tmfc1.d\t$%d, $f%d\t\t# move from fpa\n",destreg,(-srcreg))

#define MOVE_REG_TO_FP(srcreg,destreg) \
	printf("\tmtc1\t$%d, $f%d\t\t# move to fpa\n",srcreg,(-destreg))

#define MOVE_REG_TO_FP_D(srcreg,destreg) \
	printf("\tmtc1.d\t$%d, $f%d\t\t# move to fpa\n",srcreg,(-destreg))

#define STORE_TEMP_REG(off) \
	printf("\tsw\t$3, %d($sp)\t# store new value \n",off)

#define TEXTCSEG	printf("\t.text\n")

#define UPDATE_SP(off)	printf("\t%s\t$sp, %d\n",\
			       (off > 0)?"addu":"subu",(off > 0)?off:(-off))

