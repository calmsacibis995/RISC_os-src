#ident "$Header: Makefile,v 1.7 90/10/10 15:13:10 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../prom/SASH/MIPS/SASH_BO/Makefile for SASH
#

MACHINE=SASH
VERSION=MIPS
BINTYPE=BO

CCOPTS=	-O
ASOPTS=
LDOPTS=
ENDIAN=	-EB
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE= OPT
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(PI_RELEASE)
include ../../../Mkinc.sash

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
