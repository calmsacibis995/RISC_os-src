#ident "$Header: Makefile,v 1.6 90/10/10 15:16:00 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../prom/SASH/SABLE/SASH_LG/Makefile for SASH
#

MACHINE=SASH
VERSION=SABLE
BINTYPE=LG

CCOPTS=	-g
ASOPTS=	-g
LDOPTS=
ENDIAN=	-EL
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) DBG
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(PI_RELEASE)
include ../../../Mkinc.sash

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
