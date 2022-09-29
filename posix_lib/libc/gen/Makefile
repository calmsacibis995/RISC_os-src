#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.7.1.9 90/05/10 04:12:03 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
#BSD43CFLAGS=-systype posix $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
CFLAGS	=-systype posix $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS -DLIBBSD
ASFLAGS =-systype posix $(GNUM) $(INCLUDE) $(EASFLAGS) -DRISCOS
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

LOCALSRCS	= \
	abort.c atof.c cfgetispeed.c cfgetospeed.c cfsetispeed.c \
	cfsetospeed.c ctype_funcs.c isatty.c mkfifo.c \
	opendir.c \
	pathconf.c rewinddir.c \
	setlocale.c sleep.c signal.c sigsetops.c strftime.c sysconf.c \
	tcdrain.c tcflow.c tcflush.c \
	tcgetattr.c tcgetpgrp.c tcsendbreak.c tcsetattr.c tcsetpgrp.c \
	time_comm.c

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
	in_network.c

BSDCOBJS =$(BSDCSRCS:.c=.o)

SYSVGENSRCS=assert.c atoi.c atol.c bsearch.c \
	calloc.c closedir.c ctime.c ctype.c ctype_def.c \
	err_def.c errlst.c execvp.c frexp.c getcwd.c \
	getenv.c getlogin.c \
	ldexp.c \
	malloc.c memccpy.c memchr.c memcmp.c memcpy.c memset.c mktemp.c modf.c \
	perror.c qsort.c rand.c readdir.c seekdir.c strchr.c strcspn.c \
	strncat.c strncmp.c strncpy.c \
	strpbrk.c strrchr.c strspn.c strstr.c strtok.c strtol.c \
	telldir.c tolower.c toupper.c ttyname.c ttyslot.c 

SYSVGENOBJS =$(SYSVGENSRCS:.c=.o)

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
	xdr.c xdr_array.c xdr_float.c xdr_mbuf.c xdr_mem.c \
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

SYSVSTDIOSRCS = getpass.c

SYSVSTDIOOBJS =$(SYSVSTDIOSRCS:.c=.o)

SYSVGENMIPSSRCS = \
	bcmp.s

SYSVGENMIPSOBJS =$(SYSVGENMIPSSRCS:.s=.o)

BSD43GENSRCS = getpwent.c getgrent.c getgrnam.c getgrgid.c \
	ndbm.c \
	syslog.c \
	ovbcopy.c

BSD43GENOBJS =$(BSD43GENSRCS:.c=.o)

BSD43GENMIPSSRCS = \
	bcopy.s \
	ffs.s

BSD43GENMIPSOBJS =$(BSD43GENMIPSSRCS:.s=.o)

BSD43SRCS =$(SYSVSTDIOSRCS) $(BSD43NETSRCS) \
	$(BSD43NETMIPSSRCS) $(BSD43RPCSRCS) $(BSD43NAMEDSRCS) \
	$(BSD43HTSRCS) $(BSD43GENMIPSSRCS) \
	$(BSD43GENSRCS)

SYSVSRCS=$(SYSVGENSRCS) $(SYSVGENMIPSSRCS) 

SYSVOBJS=$(SYSVGENOBJS) $(SYSVGENMIPSOBJS) 

BSD43OBJS=$(SYSVSTDIOOBJS) $(BSD43NETOBJS) \
	$(BSD43NETMIPSOBJS) $(BSD43RPCOBJS) $(BSD43NAMEDOBJS) \
	$(BSD43HTOBJS) $(BSD43GENMIPSOBJS) \
	$(BSD43GENOBJS)

REMOTESRCS= $(BSD43SRCS) $(BSD43YPSRCS) \
	$(SYSVSRCS) $(BSDCSRCS) $(BSDSSRCS)

REMOTEOBJS= $(BSD43OBJS) $(BSD43YPOBJS) \
	$(SYSVOBJS) $(BSDCOBJS) $(BSDSOBJS)

SRCS= $(LOCALSRCS) $(REMOTESRCS)

OBJS= $(LOCALOBJS) $(REMOTEOBJS)

NAMEMAPPING=../include/name_mapping.h

HDRS	=

WHOAMI	=libc_gen.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

$(INTEGDIR)/$(WHOAMI): $(WHOAMI)
	-rm -f $(INTEGDIR)/$(WHOAMI)
	cp $(WHOAMI) $(INTEGDIR)

$(WHOAMI): $(OBJS) Makefile
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS) 
	-rm -f $(REMOTESRCS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

$(REMOTESRCS): Makefile

$(REMOTEOBJS): $(NAMEMAPPING)

$(BSDSSRCS) $(BSDCSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../lib/libbsd/$@"' >> $@

$(SYSVSTDIOSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../lib/libc/stdio/$@"' >> $@

$(SYSVGENMIPSSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../lib/libc/gen.mips/$@"' >> $@

$(BSD43GENMIPSSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/gen.mips/$@"' >> $@

$(BSD43NETMIPSSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/net.mips/$@"' >> $@

$(BSD43RPCSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/rpc/$@"' >> $@

$(BSD43NETSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/net/$@"' >> $@

$(BSD43GENSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/gen/$@"' >> $@

$(BSD43NAMEDSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/named/$@"' >> $@

$(BSD43HTSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/hosttable/$@"' >> $@

$(BSD43YPSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../bsd_lib/libc/yp/$@"' >> $@

$(SYSVGENSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../lib/libc/gen/$@"' >> $@

$(BSD43YPOBJS):
	$(CC) $(CFLAGS) -DSYSTYPE_SYSV -I$(DESTROOT)/usr/include/posix \
		-I$(DESTROOT)/usr/include/sysv 	\
		-I$(DESTROOT)/usr/include/sysv/bsd \
		-I../../../bsd_lib/libc/yp -c $<

$(BSD43OBJS) $(BSDSOBJS) $(BSDCOBJS) $(SYSVGENMIPSOBJS):
	$(CC) $(CFLAGS) -DSYSTYPE_SYSV -I$(DESTROOT)/usr/include/posix \
		-I$(DESTROOT)/usr/include/sysv -c $<

