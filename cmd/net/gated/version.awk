BEGIN {
	maxfields = 4;
	max = 0; strmax = "";
	for (i = 1; i <= maxfields; i++) {
		power[i] = exp(log(10)*(maxfields-i));
	}
}
{
	if (NF >= 3) {
		version = "";
		if ( $3 == "*rcsid" ) {
			version = $7;
		} 
		if ( $2 == "*" ) {
			version = $5;
		}
		if ( version == "" ) {
			continue;
		}
		sum = 0;
		num = split(version, string, ".")
		for (i = 1; i <= num; i++) {
			sum += string[i]*power[i];
		}
		if ( sum > max ) {
			max = sum;
			strmax = version;
		}
	}
}
END {
	print "char *version = \"" strmax "\";" > "version.c"
	print "Gated version is " strmax "."
}
