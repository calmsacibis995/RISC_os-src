#! /bin/csh -f
if ($#argv < 5) then
	exit 0
endif
set label="$argv[1]"
set directory="$argv[2]"
set prefix="$argv[3]"
set objlistname="$argv[4]"
echo ""
echo "$objlistname"'	= \'
@ i = 5
foreach name($argv[5-])
	set tname=$name:r
	if ("$label " == ". ") then
		set tname="${directory}/${tname}.o"
	else
		set tname="${directory}/${label}/${tname}.o"
	endif
	if ($i == $#argv) then
		echo "	$tname"
	else
		echo "	$tname"' \'
	endif
	@ i++
end
foreach name($argv[5-])
	set tname=$name:r
	if ("$label " == ". ") then
		set tname="${directory}/${tname}.o"
	else
		set tname="${directory}/${label}/${tname}.o"
	endif
	echo ""
	echo "${tname}:	${name}"
	echo "	@if [ -r ${name} ];"' \'
	echo '	then \'
	echo "		echo ${prefix} -o ${tname} -c ${name};"' \'
	echo "		${prefix} -o ${tname} -c ${name};"' \'
	echo '	else \'
	echo "		if [ ! -r ${tname} ];"' \'
	echo '		then \'
	echo "			echo ERROR: No source for ${tname} and no old .o ;"' \'
	echo '			exit 1; \'
	echo '		fi; \'
	echo '		echo "\tNo source for '"$tname"' -- using old .o"; \'
	echo '	fi'
end
exit 0
