#ident "$Header: Makefile,v 1.2.1.1 90/07/18 15:56:29 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../prom/RB3125/SABLE/PROM_BG/Makefile for prom monitor
#

MACHINE=RB3125
VERSION=SABLE
BINTYPE=BG

LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_3125_PON_$(BINTYPE)/libpon.a

CCCOPTS= -DRB3125
CASOPTS= -DRB3125

CCOPTS=	-g
ASOPTS=	-g
ENDIAN=	-EB
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) DBG
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(RB3125_ID)$(RB3125_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
