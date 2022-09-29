#ident "$Header: Makefile,v 1.5.2.1 90/07/18 15:51:54 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../M2000/SABLE/PROM_LG/Makefile for prom monitor
#

MACHINE=M2000
VERSION=SABLE
BINTYPE=LG

LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_PON_$(BINTYPE)/libpon.a

CCOPTS=	-g
ASOPTS=	-g
ENDIAN=	-EL
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) DBG
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3200_ID)$(M2000_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
