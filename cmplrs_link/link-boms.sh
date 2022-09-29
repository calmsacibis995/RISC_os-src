#! /bin/sh -f
cat ${1+"$@"} | grep '^.* l--------- ' | sed -e 's/ l--------- bin bin [0-9][0-9]* / /' | sort -u | 
	while read TARGET SOURCE REST
	do
		{
		[ -d `dirname $TARGET`/. ] || mkdir -p `dirname $TARGET`
		[ -r $TARGET ] || \
			( echo ln -s $SOURCE $TARGET ; \
			  ln -s $SOURCE $TARGET )
		}
	done
exit 0
