#include "saio/saioctl.h"

do_r(b, s, c, buf)
int		b,c;
struct reg_desc	*s;
char		*buf;
{
	int				any;
	register struct reg_desc	*rd;
	register struct reg_values	*rv;
	register unsigned		field;


	buf[0]='\0';
	if (c == 'R') {
		sprintf(buf, "0x%x", b);
	} /* if */
	any = 0;

	if (c == 'r' || b) {
		strcat(buf, "<");
		for (rd = s; rd->rd_mask; rd++) {
			field = b & rd->rd_mask;
			field = (rd->rd_shift > 0)
			    ? field << rd->rd_shift
			    : field >> -rd->rd_shift;
			if (any &&
			      (rd->rd_format || rd->rd_values
				 || (rd->rd_name && field)
			      )
			)
				strcat(buf, ",");
			if (rd->rd_name) {
				if (rd->rd_format || rd->rd_values
				    || field) {
					strcat(buf, rd->rd_name);
					any = 1;
				} /* if */
				if (rd->rd_format || rd->rd_values) {
					strcat(buf, "=");
					any = 1;
				} /* if */
			} /* if */
			if (rd->rd_format) {
				sprintf(buf+strlen(buf), rd->rd_format, field);
				any = 1;
				if (rd->rd_values)
					strcat(buf, ":");
			} /* if */
			if (rd->rd_values) {
				any = 1;
				for (rv = rd->rd_values;
				    rv->rv_name;
				    rv++) {
					if (field == rv->rv_value) {
						strcat(buf, rv->rv_name);
						break;
					} /* if */
				} /* for */
				if (rv->rv_name == NULL)
					strcat(buf, "???");
			} /* if */
		} /* for */
		strcat(buf, ">");
	} /* if */
} /* do_r */

