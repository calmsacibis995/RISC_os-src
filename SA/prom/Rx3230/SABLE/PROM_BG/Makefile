#ident "$Header: Makefile,v 1.7.2.1 90/07/18 15:49:46 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../Rx3230/SABLE/PROM_BG/Makefile for prom monitor
#

MACHINE=Rx3230
VERSION=SABLE
BINTYPE=BG

DEVSRC= saio/device/sable_3030dev.c
DEVOBJ= sable_3030dev.o
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
RELEASE = $(MAJOR_REV_ID).$(R3030_ID)$(Rx3230_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
