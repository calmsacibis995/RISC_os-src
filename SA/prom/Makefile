#ident "$Header: Makefile,v 1.10 90/10/10 13:38:05 huang Exp $"
#
# ..../prom/Makefile --- makefile to build the proms and sash
#

SHELL=/bin/sh
MAKE=/bin/make

include ../Mkinc.release
include ../Mkinc.common_defines

LIST	=	Makefile Mkinc.prom Mkinc.sash \
		M1000/Makefile \
		M1000/MIPS/Makefile \
		M1000/MIPS/PROM_BO/Makefile \
		M1000/MIPS/PROM_BG/Makefile \
		M1000/SABLE/Makefile \
		M1000/SABLE/PROM_BO/Makefile \
		M1000/SABLE/PROM_BG/Makefile \
		M1000/SABLE/PROM_LO/Makefile \
		M1000/SABLE/PROM_LG/Makefile \

		M120/Makefile \
		M120/MIPS/Makefile \
		M120/MIPS/PROM_BO/Makefile \
		M120/MIPS/PROM_BG/Makefile \
		M120/SABLE/Makefile \
		M120/SABLE/PROM_BO/Makefile \
		M120/SABLE/PROM_BG/Makefile \
		M120/SABLE/PROM_LO/Makefile \
		M120/SABLE/PROM_LG/Makefile \

		M2000/Makefile \
		M2000/MIPS/Makefile \
		M2000/MIPS/PROM_BO/Makefile \
		M2000/MIPS/PROM_BG/Makefile \
		M2000/SABLE/Makefile \
		M2000/SABLE/PROM_BO/Makefile \
		M2000/SABLE/PROM_BG/Makefile \
		M2000/SABLE/PROM_LO/Makefile \
		M2000/SABLE/PROM_LG/Makefile \

		RC6280/Makefile \
		RC6280/MIPS/Makefile \
		RC6280/MIPS/PROM_BO/Makefile \
		RC6280/MIPS/PROM_BG/Makefile \
		RC6280/SABLE/Makefile \
		RC6280/SABLE/PROM_BO/Makefile \
		RC6280/SABLE/PROM_BG/Makefile \
		RC6280/SABLE/PROM_LO/Makefile \
		RC6280/SABLE/PROM_LG/Makefile \

		Rx3230/Makefile \
		Rx3230/MIPS/Makefile \
		Rx3230/MIPS/PROM_BO/Makefile \
		Rx3230/MIPS/PROM_BG/Makefile \
		Rx3230/SABLE/Makefile \
		Rx3230/SABLE/PROM_BO/Makefile \
		Rx3230/SABLE/PROM_BG/Makefile \
		Rx3230/SABLE/PROM_LO/Makefile \
		Rx3230/SABLE/PROM_LG/Makefile \

		SASH/Makefile \
		SASH/MIPS/Makefile \
		SASH/MIPS/SASH_BO/Makefile \
		SASH/MIPS/SASH_BG/Makefile \
		SASH/SABLE/Makefile \
		SASH/SABLE/SASH_BO/Makefile \
		SASH/SABLE/SASH_BG/Makefile \
		SASH/SABLE/SASH_LO/Makefile \
		SASH/SABLE/SASH_LG/Makefile \

		../include/mips/asm.h ../include/mips/i8254clock.h \
		../include/mips/cpu.h ../include/mips/cpu_board.h \
		../include/mips/dvh.h ../include/mips/mem_board.h \
		../include/saio/parser.h ../include/saio/protocol.h \
		../include/saio/saio.h ../include/saio/saioctl.h \
		../include/saio/setjmp.h ../include/saio/stringlist.h \
		../include/sys/param.h entrypt.h prom.h \
		../saio/device/mips_2300dev.c ../saio/device/sable_2300dev.c \
		../saio/device/mips_2400dev.c ../saio/device/sable_2400dev.c \
		../saio/device/mips_3200dev.c ../saio/device/sable_3200dev.c \
		../saio/device/mips_6300dev.c ../saio/device/sable_6300dev.c \
		../saio/device/mips_3030dev.c ../saio/device/sable_3030dev.c \
		../saio/device/mips_alldev.c ../saio/device/sable_alldev.c \

		csu.mips.s clientasm.s lmem_config.s commands.c \
		machdep.c main.c promio.c 

default:	all

all remake:
		-if [ $@ = remake ]; \
		then \
			$(MAKE) tags $(MAKEARGS); \
		fi
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for prom/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done

		@echo XXXXXX Make $@ for prom/SASH XXXXXX
		@if [ -d SASH ]; \
		then \
			cd SASH; $(MAKE) $@ $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SASH does not exist: no action taken; \
		fi
		@echo prom/sash make $@ done

install_umips clean_umips clobber_umips:
		@echo XXXXXX Make $@ for prom/SASH XXXXXX
		@if [ -d SASH ]; \
		then \
			cd SASH; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SASH does not exist: no action taken; \
		fi
		@echo make $@ done

proms:
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for prom/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) prom $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done
		@echo prom make $@ done
		
clean_proms:
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for prom/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) clean $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done
		@echo prom make $@ done
		
clobber_proms:
		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for prom/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) clobber $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done
		@echo prom make $@ done
		
sash:
		@echo XXXXXX Make $@ for prom/SASH XXXXXX
		@if [ -d SASH ]; \
		then \
			cd SASH; $(MAKE) sash $(MAKEARGS); \
		else \
			echo "!!!!!! WARNING SASH does not exist: no action taken"; \
		fi
		@echo prom make $@ done
		
clean_sash:
		@echo XXXXXX Make $@ for prom/SASH XXXXXX
		@if [ -d SASH ]; \
		then \
			cd SASH; $(MAKE) clean $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SASH does not exist: no action taken; \
		fi
		@echo prom make $@ done
		
clobber_sash:
		@echo XXXXXX Make $@ for prom/SASH XXXXXX
		@if [ -d SASH ]; \
		then \
			cd SASH; $(MAKE) clobber $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SASH does not exist: no action taken; \
		fi
		@echo prom make $@ done

install:
		@echo XXXXXX Make $@ for prom/SASH XXXXXX
		@if [ -d SASH ]; \
		then \
			cd SASH; $(MAKE) install DESTDIR=$(DESTDIR) $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SASH does not exist: no action taken; \
		fi
		@echo prom make $@ done

tags:		FRC
		-rm -f tags
		P=`pwd`; D=`basename $$P`; ctags ../$$D/*.[csh] >/dev/null 2>&1

clobber clean depend:
		-if [ $@ = clobber ]; \
		then \
			rm -f errs; \
		fi

		@for i in $(MACHINES); \
		do \
			echo XXXXXX Make $@ for prom/$$i XXXXXX; \
			if [ -d $$i ]; \
			then \
				(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
			else \
				echo !!!!!! WARNING $$i does not exist: no action taken; \
			fi \
		done

		@echo XXXXXX Make $@ for prom/SASH XXXXXX
		@if [ -d SASH ]; \
		then \
			cd SASH; $(MAKE) $@ $(MAKEARGS); \
		else \
			echo !!!!!! WARNING SASH does not exist: no action taken; \
		fi
		@echo prom/sash make $@ done

print:
		$(IF_SYSV) \
			pr $(LIST) | lp; \
		$(ENDIF)
		$(IF_BSD) \
			lpr -p -J prom -C monitor $(LIST); \
		$(ENDIF)
		@echo prom/sash make print done

FRC:
