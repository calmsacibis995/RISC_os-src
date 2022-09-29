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
# $Header: keyname.sh,v 1.1.1.2 90/05/10 02:32:40 wje Exp $
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.
# ident	"@(#)curses:screen/keyname.sh	1.3"
rm -f keyname.c
echo "#include	\"curses_inc.h\"\n" > keyname.c
echo "static	char	*keystrings[] =\n\t\t{" >> keyname.c
{
    grep -v 'KEY_F(' keycaps | awk '{ print $5, $4 }' | sed -e 's/,//g' -e 's/KEY_//'
    # These three aren't in keycaps
    echo '0401 BREAK\n0530 SRESET\n0531 RESET'
} |  sort -n | awk '
    {
	print "\t\t    \"" $2 "\",	/* " $1 " */"
    }
' >> keyname.c

LAST=`tail -1 keyname.c | awk -F'"' '{print $2}'`
cat << ! >> keyname.c
		};

char	*keyname(key)
int	key;
{
    static	char	buf[16];

    if (key >= 0400)
    {
	register	int	i;

	if ((key == 0400) || (key > KEY_${LAST}))
	    return ("UNKNOWN KEY");
	if (key > 0507)
	    i = key - (0401 + ((0507 - 0410) + 1));
	else
	    if (key >= 0410)
	    {
		(void) sprintf(buf, "KEY_F(%d)", key - 0410);
		goto ret_buf;
	    }
	    else
		i = key - 0401;
	(void) sprintf(buf, "KEY_%s", keystrings[i]);
	goto ret_buf;
    }

    if (key >= 0200)
    {
	if (SHELLTTY.c_cflag & CS8)
	    (void) sprintf(buf, "%c", key);
	else
	    (void) sprintf(buf, "M-%s", unctrl(key & 0177));
	goto ret_buf;
    }

    if (key < 0)
    {
	(void) sprintf(buf, "%d", key);
ret_buf:
	return (buf);
    }

    return (unctrl(key));
}
!
exit 0
