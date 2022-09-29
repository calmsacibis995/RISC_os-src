#ident "$Header: Makefile,v 1.9 89/06/15 20:26:34 clp Exp $"

include ../Mkinc.common_defines

# BINDIRS : the binaries that we can build
BINDIRS= MIPS_STAND_O MIPS_STAND SABLE_STAND_B SABLE_STAND_BO \
	SABLE_STAND_L SABLE_STAND_LO

# BLDDIRS : the binaries that we will build
BLDDIRS= MIPS_STAND_O

LIST	=	Makefile Mkinc.stand \
		MIPS_STAND/Makefile MIPS_STAND_O/Makefile \
		SABLE_STAND_B/Makefile SABLE_STAND_BO/Makefile \
		SABLE_STAND_L/Makefile SABLE_STAND_LO/Makefile \
		format.c qsort.c diag.c

default:	all

all remake :
		-if [ $@ = remake ]; \
		then \
			$(MAKE) tags $(MAKEARGS); \
		fi
		@for i in $(BLDDIRS); \
		do \
			echo XXXXXX Make $@ for stand/$$i XXXXXX; \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		done
		@echo stand make $@ done

install_umips:	
		@for i in $(BLDDIRS); \
		do \
			echo XXXXXX Make $@ for stand/$$i XXXXXX; \
			(cd $$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		done
		@echo stand make $@ done


clean_umips:	
		@for i in $(BLDDIRS); \
		do \
		    echo XXXXXX Make $@ for stand/$$i XXXXXX; \
		    (cd $$i; $(MAKE) clean DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		done
		@echo stand make $@ done

format:	
		echo XXXXXX Make $@ XXXXXX; \
		(cd MIPS_STAND_O; $(MAKE) format DESTDIR=$(DESTDIR)); \
		echo stand make $@ done

format_debug:	
		echo XXXXXX Make $@ XXXXXX; \
		(cd MIPS_STAND; $(MAKE) format DESTDIR=$(DESTDIR));\
		echo stand make $@ done


clobber_umips:	
		@for i in $(BLDDIRS); \
		do \
		    echo XXXXXX Make $@ for stand/$$i XXXXXX; \
		    (cd $$i; $(MAKE) clobber DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		done
		@echo stand make $@ done


install:
		@for i in $(BLDDIRS); \
		do \
			echo XXXXXX Make $@ for stand/$$i XXXXXX; \
			(cd $$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		done
		@echo stand make $@ done

tags:		FRC
		-rm -f tags
		P=`pwd`; D=`basename $$P`; ctags ../$$D/*.[csh] >/dev/null 2>&1

clobber clean depend:
		-if [ $@ = clobber ]; \
		then \
			rm -f errs; \
		fi
		@for i in $(BINDIRS); \
		do \
			echo XXXXXX Make $@ for stand/$$i XXXXXX; \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		done
		@echo stand make $@ done

print:
		$(IF_SYSV) \
			pr $(LIST) | lp; \
		$(ENDIF)
		$(IF_BSD) \
			lpr -p -J standalone -C programs $(LIST); \
		$(ENDIF)
		@echo stand make print done

FRC:
