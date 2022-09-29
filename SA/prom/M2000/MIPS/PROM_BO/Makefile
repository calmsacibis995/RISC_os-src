#ident "$Header: Makefile,v 1.5.2.1 90/07/18 15:51:10 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../prom/M2000/MIPS/PROM_BO/Makefile for prom monitor
#

MACHINE=M2000
VERSION=MIPS
BINTYPE=BO

LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_PON_$(BINTYPE)/libpon.a

CCCOPTS= -DR3200
CASOPTS= -DR3200

CCOPTS=	-O
ASOPTS=
LDOPTS=
ENDIAN=	-EB
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) OPT
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3200_ID)$(M2000_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
