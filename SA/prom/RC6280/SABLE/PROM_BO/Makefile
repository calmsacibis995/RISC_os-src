#ident "$Header: Makefile,v 1.6.2.1 90/07/18 15:55:08 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../RC6280/SABLE/PROM_BO/Makefile for prom monitor
#

MACHINE=RC6280
VERSION=SABLE
BINTYPE=BO

DEVSRC= saio/device/sable_6300dev.c
DEVOBJ= sable_6300dev.o
LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_6000_PON_$(BINTYPE)/libpon.a

CCOPTS=	-O
ASOPTS=
ENDIAN=	-EB
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) OPT
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R6300_ID)$(RC6280_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
