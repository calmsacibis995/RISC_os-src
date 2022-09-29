#ident "$Header: Makefile,v 1.5.2.1 90/07/18 15:48:02 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../M120/SABLE/PROM_BO/Makefile for prom monitor
#

MACHINE=M120
VERSION=SABLE
BINTYPE=BO

LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_PON_$(BINTYPE)/libpon.a

CCOPTS=	-O
ASOPTS=
ENDIAN=	-EB
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) OPT
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE= $(MAJOR_REV_ID).$(R2400_ID)$(M120_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
