#ident "$Header: Makefile,v 1.7.1.1 90/07/18 15:54:37 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../prom/RC6280/MIPS/PROM_BO/Makefile for prom monitor
#

MACHINE=RC6280
VERSION=MIPS
BINTYPE=BO

DIAGMON_START=	bfc40000

DEVSRC= saio/device/mips_6300dev.c
DEVOBJ= mips_6300dev.o
LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_6000_PON_$(BINTYPE)/libpon.a

CCOPTS=	-O
ASOPTS=
ASOPTS=	-DDIAGMON_START=0x$(DIAGMON_START)
LDOPTS=
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
