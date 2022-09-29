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
/* $Header: searchdir.s,v 1.4.5.2 90/05/10 05:44:04 wje Exp $ */
/*	searchdir(buf, n, target) - search a directory for target
 *	returns offset of match, or emtpy slot, or -1
 */

#define buffer	a0
#define n	a1
#define target	a2
#define empty	a3
#define bufchar	t0
#define tarchar	t1
#define tar1	t2
#define sixteen	t3
#define buf	t4

LEAF(searchdir)
	move	empty,zero		# set emtpy marker
	lb	tar1,0(target)		# get first char of target
	li	sixteen,16
	move	buf,buffer

1:	blt	n,sixteen,2f		# check remaining size
	lh	bufchar,0(buf)
	beqz	bufchar,3f		# empty slot ?
	lb	bufchar,2(buf)
	bne	bufchar,tar1,4f

#define CHECK(off) \
	lb	bufchar,off+2(buf)	;\
	lb	tarchar,off(target)	;\
	bne	bufchar,tarchar,4f	;\
	beqz	tarchar,5f		# match on 0

	CHECK(1)
	CHECK(2)
	CHECK(3)
	CHECK(4)
	CHECK(5)
	CHECK(6)
	CHECK(7)
	CHECK(8)
	CHECK(9)
	CHECK(10)
	CHECK(11)
	CHECK(12)
	lb	bufchar,15(buf)
	lb	tarchar,13(target)
	beq	bufchar,tarchar,5f

4:	# continue
	addu	buf,sixteen,buf
	subu	n,n,sixteen
	b	1b
3:	# empty
	bnez	empty,4b	# need empty slot ?
	move	empty,buf	# save offset of empty slot
	b	4b
5:	# match
	subu	v0,buf,buffer	# calc offset to buf
	j	ra
2:	# done
	li	v0,-1		# error return
	beqz	empty,6f	# empty buffer found ?
	subu	v0,empty,buffer # yes, calc offset to empty slot
6:	j	ra
	END(searchdir)
