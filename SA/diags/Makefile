#ident "$Header: Makefile,v 1.6.1.1 90/07/18 14:26:33 huang Exp $"

include	../Mkinc.common_defines

DIAG_COMPONENTS= \
		pon

default:	all

all depend remake clean clobber tags print proms clean_proms clobber_proms:
	@for i in $(DIAG_COMPONENTS); \
	do \
		echo XXXXXX Make $@ for $$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done

