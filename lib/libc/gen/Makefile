# |-----------------------------------------------------------|
# | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
# 
# $Header: Makefile,v 1.14.1.9 90/05/09 14:53:11 wje Exp $ 

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(GNUM) -systype sysv $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =$(GNUM) -systype sysv $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

LOCALSRCS	= \
	a64l.c abort.c abs.c assert.c atof.c atoi.c atol.c bsearch.c \
	calloc.c clock.c closedir.c crypt.c ctime.c ctype.c ctype_def.c \
	dial.c drand48.c dup2.c ecvt.c err_def.c errlst.c execvp.c \
	frexp.c ftok.c ftw.c gcvt.c getcwd.c getenv.c \
	getlogin.c getopt.c getpw.c \
	getut.c grent_def.c hsearch.c isatty.c l3.c l64a.c \
	ldexp.c lfind.c lsearch.c mall_def.c malloc.c memccpy.c memchr.c \
	memcmp.c memcpy.c memset.c mktemp.c modf.c mon.c opendir.c \
	opt_data.c perror.c putenv.c putpwent.c pwent_def.c qsort.c rand.c \
	readdir.c seekdir.c sleep.c ssignal.c strcat.c strchr.c strcmp.c \
	strcpy.c strcspn.c strdup.c strlen.c strncat.c strncmp.c strncpy.c \
	strpbrk.c strrchr.c strspn.c strtod.c strtok.c strtol.c swab.c \
	tell.c telldir.c tfind.c tolower.c toupper.c tsearch.c ttyname.c \
	ttyslot.c cftime.c free_def.c old_tolower.c old_toupper.c \
	reall_def.c time_comm.c alloca.c gtty.c stty.c strstr.c

LOCALOBJS=$(LOCALSRCS:.c=.o)

BSDSSRCS	= \
	index.s \
	bzero.s \
	rindex.s \
	getdomnm.s \
	gettimeofday.s \
	sendto.s \
	select.s \
	recv.s \
	recvfrom.s \
	flock.s \
	getsocknm.s \
	socket.s \
	connect.s \
	bind.s \
	gethostnm.s \
	send.s \
	accept.s \
	getgroups.s \
	listen.s \
	setreuid.s \
	setsockopt.s \
	Ovfork.s

BSDSOBJS =$(BSDSSRCS:.s=.o)

BSDCSRCS	= \
	in_ntoa.c \
	strcasecmp.c \
	in_addr.c \
	in_mkaddr.c \
	in_network.c \
	getdtabsz.c

BSDCOBJS =$(BSDCSRCS:.c=.o)

BSD43NETSRCS =bindresvport.c rcmd.c vis.c res_comp.c res_debug.c \
	res_init.c res_mkquery.c res_query.c res_send.c \
	ether_addr.c gethostnamadr.c getnetbyaddr.c getnetbyname.c \
	getnetent.c getnetgrent.c getproto.c getprotoent.c getprotoname.c \
	getrpcent.c getservent.c innetgr.c ruserpass.c sethostent.c 

BSD43NETOBJS =$(BSD43NETSRCS:.c=.o)

BSD43NETMIPSSRCS =ntohl.s ntohs.s htonl.s htons.s

BSD43NETMIPSOBJS =$(BSD43NETMIPSSRCS:.s=.o)

BSD43RPCSRCS	=auth_des.c auth_none.c auth_unix.c authdes_prot.c \
	authunix_prot.c clnt_generic.c \
	clnt_perror.c clnt_raw.c clnt_simple.c clnt_tcp.c clnt_udp.c \
	get_myaddress.c key_call.c key_prot.c netname.c \
	openchild.c pmap_clnt.c pmap_getmaps.c pmap_getport.c \
	pmap_prot.c pmap_prot2.c pmap_rmt.c rpc_callmsg.c rpc_prot.c \
	rpccommondata.c rpcdtablesize.c rtime.c \
	svc.c svc_auth.c svc_auth_unix.c svc_raw.c svc_run.c \
	svc_simple.c svc_tcp.c svc_udp.c svcauth_des.c \
	xdr.c xdr_array.c xdr_float.c xdr_mem.c \
	xdr_rec.c xdr_reference.c xdr_stdio.c

BSD43RPCOBJS =$(BSD43RPCSRCS:.c=.o)

BSD43NAMEDSRCS = dns_getnamadr.c dns_service.c dns_sethent.c

BSD43NAMEDOBJS =$(BSD43NAMEDSRCS:.c=.o)

BSD43HTSRCS = ht_getgrent.c ht_getgrgid.c ht_getgrnam.c ht_gethent.c \
	ht_getnamadr.c ht_getpent.c ht_getpname.c ht_getproto.c ht_getpwent.c \
	ht_gnbyaddr.c ht_gnbyname.c ht_gnetent.c ht_gngrent.c ht_grpcent.c \
	ht_gservent.c ht_innetgr.c ht_service.c

BSD43HTOBJS =$(BSD43HTSRCS:.c=.o)

BSD43YPSRCS = yp_bind.c \
	yp_enum.c yp_master.c yp_match.c \
	yp_order.c yp_update.c yperr_string.c ypmaint_xdr.c ypprot_err.c \
	ypv1_xdr.c ypxdr.c ypupdate_xdr.c yp_getgrent.c yp_gethostent.c \
	yp_getpent.c yp_getpwent.c yp_getrpcent.c yp_getservent.c yp_gnetent.c \
	yp_gnetgr.c yp_innetgr.c yp_service.c yp_all.c

BSD43YPOBJS =$(BSD43YPSRCS:.c=.o)

BSD43GENSRCS = getpwent.c getgrent.c getgrnam.c getgrgid.c \
	ndbm.c \
	syslog.c \
	ovbcopy.c

BSD43GENOBJS =$(BSD43GENSRCS:.c=.o)

BSD43GENMIPSSRCS = \
	bcopy.s \
	ffs.s

BSD43GENMIPSOBJS =$(BSD43GENMIPSSRCS:.s=.o)

BSD43SRCS =$(BSD43GENSRCS) $(BSD43GENMIPSSRCS) $(BSD43NETSRCS) \
	$(BSD43NETMIPSSRCS) $(BSD43RPCSRCS) $(BSD43NAMEDSRCS) \
	$(BSD43HTSRCS)

BSD43OBJS =$(BSD43GENOBJS) $(BSD43GENMIPSOBJS) $(BSD43NETOBJS) \
	$(BSD43NETMIPSOBJS) $(BSD43RPCOBJS) $(BSD43NAMEDOBJS) \
	$(BSD43HTOBJS)

SRCS= $(LOCALSRCS) $(BSD43SRCS) $(BSD43YPSRCS) $(BSDCSRCS) $(BSDSSRCS)

OBJS= $(LOCALOBJS) $(BSD43OBJS) $(BSD43YPOBJS) $(BSDCOBJS) $(BSDSOBJS)

HDRS	=

WHOAMI	=libc_gen.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

$(INTEGDIR)/$(WHOAMI): $(WHOAMI)
	-rm -f $(INTEGDIR)/$(WHOAMI)
	cp $(WHOAMI) $(INTEGDIR)

$(WHOAMI): $(OBJS)
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI) $(BSD43SRCS) $(BSD43YPSRCS) $(BSDCSRCS) $(BSDSSRCS)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
	$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

$(BSDSSRCS) $(BSDCSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../libbsd/$@"' >> $@

$(BSD43GENSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/gen/$@"' >> $@

$(BSD43GENMIPSSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/gen.mips/$@"' >> $@

$(BSD43NETMIPSSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/net.mips/$@"' >> $@

$(BSD43RPCSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/rpc/$@"' >> $@

$(BSD43NETSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/net/$@"' >> $@

$(BSD43NAMEDSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/named/$@"' >> $@

$(BSD43HTSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/hosttable/$@"' >> $@

$(BSD43YPSRCS):	../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/yp/$@"' >> $@

getpw.c: ../include/name_mapping.h
	[ ! -r $@ ] || rm -f $@
	echo '#include "../include/name_mapping.h"' > $@
	echo '#include "../../../bsd_lib/libc/compat-4.1/$@"' >> $@

$(BSD43YPOBJS):  $(BSD43YPSRCS)
	$(CC) $(CFLAGS) $(ECFLAGS) -I../../../bsd_lib/libc/yp \
		-I$(DESTROOT)/usr/include/bsd -c $<
