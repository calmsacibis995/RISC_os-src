#ident "$Header: Makefile,v 1.5 89/11/08 09:02:04 huang Exp $"
#	%Q%	%I%	%M%
#
# Copyright 1985 by MIPS Computer Systems, Inc.
#
# Standalone program Makefile
#

VERSION=SABLE
BINTYPE=LG
LIBPATH=../../saio/SAIO/$(VERSION)/SAIO_$(BINTYPE)
LIBSA=	$(LIBPATH)/libsa.a
CSU=	$(LIBPATH)/csu.mips.o
CCOPTS=	-g
ASOPTS=	-g
LDOPTS=
ENDIAN=	-EL

include ../../Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(PI_RELEASE)
include ../Mkinc.stand

# DO NOT DELETE THIS LINE (or the line after this one)

format.o: ../format.c ../../include/sys/param.h ../../include/machine/param.h
format.o: ../../include/signal.h ../../include/sys/types.h
format.o: ../../include/sys/inode.h ../../include/sys/fs.h
format.o: ../../include/sys/dir.h ../../include/sys/file.h
format.o: ../../include/sys/errno.h ../../include/netinet/in.h
format.o: ../../include/machine/dvh.h ../../include/mips/cpu_board.h
format.o: ../../include/saio/tpd.h ../../include/saio/saio.h
format.o: ../../include/saio/saioctl.h ../../include/sys/dkio.h
format.o: ../../include/sys/gen_ioctl.h ../../include/saio/ctype.h
qsort.o: ../qsort.c
