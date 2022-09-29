#ident "$Header: Makefile,v 1.5.2.1 90/07/18 15:47:11 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../prom/M120/MIPS/PROM_BG/Makefile for prom monitor
#

MACHINE=M120
VERSION=MIPS
BINTYPE=BG

LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_PON_$(BINTYPE)/libpon.a

CCOPTS=	-g
ASOPTS=	-g
ENDIAN=	-EB
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) DBG
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE= $(MAJOR_REV_ID).$(R2400_ID)$(M120_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
