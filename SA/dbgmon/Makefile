#ident "$Header: Makefile,v 1.6 90/10/24 10:38:46 huang Exp $"
SHELL=/bin/sh

include ../Mkinc.common_defines

BINDIRS= MIPS_O MIPS SABLE_B SABLE_BO SABLE_L SABLE_LO
BLDDIRS= MIPS_O

LIST	=	Makefile Mkinc.dbgmon \
		MIPS/Makefile MIPS_O/Makefile \
		SABLE_B/Makefile SABLE_BO/Makefile \
		SABLE_L/Makefile SABLE_LO/Makefile \
		../include/mips/asm.h ../include/mips/cpu.h \
		../include/saio/setjmp.h ../include/saio/parser.h \
		../include/saio/protoio.h ../include/saio/protocol.h \
		../include/saio/saioctl.h ../include/saio/stringlist.h \
		../include/prom/entrypt.h dbgmon.h inst.h csu.mips.s \
		faultasm.s promlink.s tlb.s brkpt.c commands.c ctype.c \
		dbgmon.c disMips.c fault.c machdep.c rdebug.c regdesc.c

default: all

all remake:
		-if [ $@ = remake ]; \
		then \
			$(MAKE) tags $(MAKEARGS); \
		fi
		@for i in $(BLDDIRS); \
		do \
			echo XXXXXX Make $@ for dbgmon/$$i XXXXXX; \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		done
		@echo dbgmon make $@ done

install install_umips:
		cd MIPS_O; $(MAKE) install DESTDIR=$(DESTDIR) $(MAKEARGS)

clean_umips:
		cd MIPS_O; $(MAKE) clean $(MAKEARGS)
		@echo dbgmon make $@ done

clobber_umips:
		rm -f errs
		cd MIPS_O; $(MAKE) clobber $(MAKEARGS)
		@echo dbgmon make $@ done

tags:		FRC
		-rm -f tags
		P=`pwd`; D=`basename $$P`; ctags ../$$D/*.[csh] >/dev/null 2>&1

clean depend clobber:
		-if [ $@ = clobber ]; \
		then \
			rm -f errs; \
		fi
		@for i in $(BINDIRS); \
		do \
			echo XXXXXX Make $@ for dbgmon/$$i XXXXXX; \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		done
		@echo dbgmon make $@ done

print:	
		$(IF_SYSV) \
			pr $(LIST) | lp; \
		$(ENDIF)
		$(IF_BSD) \
			lpr -p -J debug -C monitor $(LIST); \
		$(ENDIF)
		@echo dbgmon make print done

FRC:
