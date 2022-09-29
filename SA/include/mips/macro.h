#ident "$Header: macro.h,v 1.2 90/01/23 14:16:41 huang Exp $"
/* $Copyright$ */

#define set_all_registers( value )		addiu	r1,r0,value;	\
		addiu	r2,r0,value;					\
		addiu	r3,r0,value;					\
		addiu	r4,r0,value;					\
		addiu	r5,r0,value;					\
		addiu	r6,r0,value;					\
		addiu	r7,r0,value;					\
		addiu	r8,r0,value;					\
		addiu	r9,r0,value;					\
		addiu	r10,r0,value;					\
		addiu	r11,r0,value;					\
		addiu	r12,r0,value;					\
		addiu	r13,r0,value;					\
		addiu	r14,r0,value;					\
		addiu	r15,r0,value;					\
		addiu	r16,r0,value;					\
		addiu	r17,r0,value;					\
		addiu	r18,r0,value;					\
		addiu	r19,r0,value;					\
		addiu	r20,r0,value;					\
		addiu	r21,r0,value;					\
		addiu	r22,r0,value;					\
		addiu	r23,r0,value;					\
		addiu	r24,r0,value;					\
		addiu	r25,r0,value;					\
		addiu	r26,r0,value;					\
		addiu	r27,r0,value;					\
		addiu	r28,r0,value;					\
		addiu	r29,r0,value;					\
		addiu	r30,r0,value;					\
		addiu	r31,r0,value;

#define BOUNCE_CURSOR(x,y)    if(!((int)x % 0x8000) && (int)x != 0xa0800000){ \
					if(y){ \
						printf("\b"); \
						y = 0; \
					} \
					else{	\
						printf(" "); \
						y = 1; \
					} \
				}

