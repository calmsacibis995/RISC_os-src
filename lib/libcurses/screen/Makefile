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
# $Header: Makefile,v 1.2.1.3 90/05/09 20:03:57 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

DEBUG	=
MAXOPT	=-O1
INCLUDE	=-I../include
CFLAGS	=$(DEBUG) $(GNUM) $(MAXOPT) -DNOBLIT $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =$(DEBUG) $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/usr/lib

SRCS	=_addch.c _addchnstr.c _addchstr.c _addnstr.c _addstr.c \
	_attroff.c _attron.c _attrset.c _beep.c _bkgd.c \
	_bkgdset.c _border.c _box.c _clear.c _clrtobot.c \
	_clrtoeol.c _crmode.c _del_curterm.c _delch.c _deleteln.c \
	_echo.c _echochar.c _erase.c _fixterm.c _flash.c \
	_garbagdlins.c _garbagedwin.c _getch.c _getstr.c \
	_halfdelay.c _hline.c _inch.c _inchnstr.c _inchstr.c \
	_innstr.c _insch.c _insdelln.c _insertln.c _insnstr.c \
	_insstr.c _instr.c _intrflush.c _meta.c _move.c \
	_mvaddch.c _mvaddchnstr.c _mvaddchstr.c _mvaddnstr.c \
	_mvaddstr.c _mvdelch.c _mvgetch.c _mvgetstr.c _mvhline.c \
	_mvinch.c _mvinchnstr.c _mvinchstr.c _mvinnstr.c \
	_mvinsch.c _mvinsnstr.c _mvinsstr.c _mvinstr.c _mvvline.c \
	_mvwaddch.c _mvwaddchnst.c _mvwaddchstr.c _mvwaddnstr.c \
	_mvwaddstr.c _mvwdelch.c _mvwgetch.c _mvwgetstr.c \
	_mvwhline.c _mvwinch.c _mvwinchnst.c _mvwinchstr.c \
	_mvwinnstr.c _mvwinsch.c _mvwinsnstr.c _mvwinsstr.c \
	_mvwinstr.c _mvwvline.c _newterm.c _nl.c _nocrmode.c \
	_noecho.c _nonl.c _noqiflush.c _overlay.c _overwrite.c \
	_qiflush.c _refresh.c _resetterm.c _saveterm.c \
	_scr_init.c _scr_restore.c _scr_set.c _scrl.c _scroll.c \
	_set_curterm.c _set_term.c _setscrreg.c _slk_init.c \
	_standend.c _standout.c _subpad.c _timeout.c _touchline.c \
	_unctrl.c _vline.c _waddchstr.c _waddstr.c _wclear.c \
	_wdeleteln.c _werase.c _winsertln.c _winsstr.c \
	_wstandend.c _wstandout.c V2.__sscans.c V2._sprintw.c \
	V2.makenew.c V3.box.c V3.initscr.c V3.m_addch.c V3.m_addstr.c \
	V3.m_clear.c V3.m_erase.c V3.m_initscr.c V3.m_move.c V3.m_newterm.c \
	V3.m_refresh.c V3.newterm.c V3.pechochar.c V3.upd_old_y.c \
	V3.vidattr.c V3.vidputs.c V3.waddch.c V3.wattroff.c V3.wattron.c \
	V3.wattrset.c V3.wechochar.c V3.winsch.c baudrate.c cbreak.c \
	chkinput.c clearok.c copywin.c curs_set.c \
	curserr.c curses.c def_prog.c delay.c delay_out.c \
	delkey.c delkeymap.c delscreen.c delterm.c delwin.c \
	derwin.c doupdate.c draino.c dupwin.c endwin.c \
	erasechar.c flushinp.c getattrs.c getbegyx.c getbkgd.c \
	getmaxyx.c getparyx.c getsyx.c gettmode.c getwin.c \
	getyx.c has_ic.c has_il.c idcok.c idlok.c \
	immedok.c init_acs.c init_costs.c initscr.c is_linetou.c \
	is_wintou.c keypad.c killchar.c leaveok.c \
	longname.c makenew.c memSset.c meta.c mvcur.c \
	mvderwin.c mvprintw.c mvscanw.c mvwin.c mvwprintw.c \
	mvwscanw.c napms.c newkey.c newpad.c newscreen.c \
	newwin.c nocbreak.c nodelay.c noraw.c \
	notimeout.c outch.c overlap.c pechochar.c pnoutref.c \
	prefresh.c printw.c putp.c putwin.c quick_echo.c \
	raw.c redrawwin.c reset_sh.c resetty.c restart.c \
	ring.c ripoffline.c savetty.c scanw.c scr_all.c \
	scr_dump.c scr_ll_dump.c scr_reset.c scrollok.c setcurscreen.c \
	setcurterm.c setecho.c setnonl.c setqiflush.c \
	setsyx.c setterm.c slk_atroff.c slk_atron.c \
	slk_atrset.c slk_clear.c slk_label.c \
	slk_noutref.c slk_refresh.c slk_restore.c slk_set.c slk_start.c \
	slk_touch.c subwin.c syncok.c tcsearch.c termattrs.c \
	termerr.c termname.c tgetch.c tgoto.c \
	tinputfd.c \
	touchwin.c tparm.c tputs.c trace.c \
	tstp.c ttimeout.c typeahead.c unctrl.c ungetch.c \
	untouchwin.c vidputs.c vidupdate.c vsscanf.c vwprintw.c \
	vwscanw.c waddch.c waddchnstr.c waddnstr.c wattroff.c \
	wattron.c wattrset.c wbkgd.c wbkgdset.c wborder.c \
	wclrtobot.c wclrtoeol.c wdelch.c wechochar.c wgetch.c \
	wgetstr.c whline.c winch.c winchnstr.c winchstr.c \
	winnstr.c winsch.c winsdelln.c winsnstr.c winstr.c \
	wmove.c wnoutref.c wprintw.c wredrawln.c wrefresh.c \
	wscanw.c wscrl.c wsetscrreg.c wsyncdown.c wsyncup.c \
	wtimeout.c wtouchln.c wvline.c \
	can_change.c color_cont.c init_color.c init_pair.c has_colors.c \
	pair_cont.c start_col.c

SETUPSRC=termcap.c tifget.c tifnames.c tiget.c tinames.c tnames.c \
	keyname.c keycaps setupterm.c setkeymap.c keyname.sh

BSDSRCS =select.s

OBJS	=_addch.o _addchnstr.o _addchstr.o _addnstr.o _addstr.o \
	_attroff.o _attron.o _attrset.o _beep.o _bkgd.o \
	_bkgdset.o _border.o _box.o _clear.o _clrtobot.o \
	_clrtoeol.o _crmode.o _del_curterm.o _delch.o _deleteln.o \
	_echo.o _echochar.o _erase.o _fixterm.o _flash.o \
	_garbagdlins.o _garbagedwin.o _getch.o _getstr.o \
	_halfdelay.o _hline.o _inch.o _inchnstr.o _inchstr.o \
	_innstr.o _insch.o _insdelln.o _insertln.o _insnstr.o \
	_insstr.o _instr.o _intrflush.o _meta.o _move.o \
	_mvaddch.o _mvaddchnstr.o _mvaddchstr.o _mvaddnstr.o \
	_mvaddstr.o _mvdelch.o _mvgetch.o _mvgetstr.o _mvhline.o \
	_mvinch.o _mvinchnstr.o _mvinchstr.o _mvinnstr.o \
	_mvinsch.o _mvinsnstr.o _mvinsstr.o _mvinstr.o _mvvline.o \
	_mvwaddch.o _mvwaddchnst.o _mvwaddchstr.o _mvwaddnstr.o \
	_mvwaddstr.o _mvwdelch.o _mvwgetch.o _mvwgetstr.o \
	_mvwhline.o _mvwinch.o _mvwinchnst.o _mvwinchstr.o \
	_mvwinnstr.o _mvwinsch.o _mvwinsnstr.o _mvwinsstr.o \
	_mvwinstr.o _mvwvline.o _newterm.o _nl.o _nocrmode.o \
	_noecho.o _nonl.o _noqiflush.o _overlay.o _overwrite.o \
	_qiflush.o _refresh.o _resetterm.o _saveterm.o \
	_scr_init.o _scr_restore.o _scr_set.o _scrl.o _scroll.o \
	_set_curterm.o _set_term.o _setscrreg.o _slk_init.o \
	_standend.o _standout.o _subpad.o _timeout.o _touchline.o \
	_unctrl.o _vline.o _waddchstr.o _waddstr.o _wclear.o \
	_wdeleteln.o _werase.o _winsertln.o _winsstr.o \
	_wstandend.o _wstandout.o V2.__sscans.o V2._sprintw.o \
	V2.makenew.o V3.box.o V3.initscr.o V3.m_addch.o V3.m_addstr.o \
	V3.m_clear.o V3.m_erase.o V3.m_initscr.o V3.m_move.o V3.m_newterm.o \
	V3.m_refresh.o V3.newterm.o V3.pechochar.o V3.upd_old_y.o \
	V3.vidattr.o V3.vidputs.o V3.waddch.o V3.wattroff.o V3.wattron.o \
	V3.wattrset.o V3.wechochar.o V3.winsch.o baudrate.o cbreak.o \
	chkinput.o clearok.o copywin.o curs_set.o \
	curserr.o curses.o def_prog.o delay.o delay_out.o \
	delkey.o delkeymap.o delscreen.o delterm.o delwin.o \
	derwin.o doupdate.o draino.o dupwin.o endwin.o \
	erasechar.o flushinp.o getattrs.o getbegyx.o getbkgd.o \
	getmaxyx.o getparyx.o getsyx.o gettmode.o getwin.o \
	getyx.o has_ic.o has_il.o idcok.o idlok.o \
	immedok.o init_acs.o init_costs.o initscr.o is_linetou.o \
	is_wintou.o keypad.o killchar.o leaveok.o \
	longname.o makenew.o memSset.o meta.o mvcur.o \
	mvderwin.o mvprintw.o mvscanw.o mvwin.o mvwprintw.o \
	mvwscanw.o napms.o newkey.o newpad.o newscreen.o \
	newwin.o nocbreak.o nodelay.o noraw.o \
	notimeout.o outch.o overlap.o pechochar.o pnoutref.o \
	prefresh.o printw.o putp.o putwin.o quick_echo.o \
	raw.o redrawwin.o reset_sh.o resetty.o restart.o \
	ring.o ripoffline.o savetty.o scanw.o scr_all.o \
	scr_dump.o scr_ll_dump.o scr_reset.o scrollok.o setcurscreen.o \
	setcurterm.o setecho.o setnonl.o setqiflush.o \
	setsyx.o setterm.o slk_atroff.o slk_atron.o \
	slk_atrset.o slk_clear.o slk_label.o \
	slk_noutref.o slk_refresh.o slk_restore.o slk_set.o slk_start.o \
	slk_touch.o subwin.o syncok.o tcsearch.o termattrs.o \
	termerr.o termname.o tgetch.o tgoto.o \
	tinputfd.o \
	touchwin.o tparm.o tputs.o trace.o \
	tstp.o ttimeout.o typeahead.o unctrl.o ungetch.o \
	untouchwin.o vidputs.o vidupdate.o vsscanf.o vwprintw.o \
	vwscanw.o waddch.o waddchnstr.o waddnstr.o wattroff.o \
	wattron.o wattrset.o wbkgd.o wbkgdset.o wborder.o \
	wclrtobot.o wclrtoeol.o wdelch.o wechochar.o wgetch.o \
	wgetstr.o whline.o winch.o winchnstr.o winchstr.o \
	winnstr.o winsch.o winsdelln.o winsnstr.o winstr.o \
	wmove.o wnoutref.o wprintw.o wredrawln.o wrefresh.o \
	wscanw.o wscrl.o wsetscrreg.o wsyncdown.o wsyncup.o \
	wtimeout.o wtouchln.o wvline.o \
	can_change.o color_cont.o init_color.o init_pair.o has_colors.o \
	pair_cont.o start_col.o \
	termcap.o tifget.o tifnames.o tiget.o tinames.o tnames.o \
	keyname.o setupterm.o setkeymap.o select.o

HDRS	=

LIBSUFFIX=
WHOAMI	=libcurses$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
LINKS	=-L $(DESTROOT)/usr/lib/libtermcap.a -L $(DESTROOT)/usr/lib/libtermlib.a
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)
	-rm -f $(BSDSRCS) $(SETUPSRC)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) \
		-L /usr/lib/libtermcap.a -L /usr/lib/libtermlib.a

$(BSDSRCS): ../../libbsd/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../libbsd/$@ $@

$(SETUPSRC): ../setup/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../setup/$@ $@
