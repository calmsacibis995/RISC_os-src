#!/bin/sh
#
# $Header: inst_subsl.sh,v 2.4.1.11.1.2 90/07/11 18:23:48 hawkes Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1986 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------

case "$Instenv" in
  "") . inst_env ;;
  esac

# force shell to keep quite
set +x
		
if [ "$Os2" = "y" ]
then
case "$Onmini" in
  y) echo " "
     echo "Note: It is not necessary to install a kernel on the miniroot"
     echo "      The process will take a few minutes."
     ask "Would you like to install the kernel to the miniroot" n y n
     case $Ans in
       y) echo "Forward spacing the tape $TGfsfnumber filesets..."
        rewind
        fsf $TGfsfnumber
        echo "Reading the $TGunix kernel from tape..."
        dd if=$Taperw bs=16k | dd of=/$TGunix bs=512 skip=1
        ln -s /$TGunix /unix 
        echo "Done."
        ## TGfsfnumber is the # of filesets to skip
        ## TGunix is the unix name
        ;;
     esac
     ;;
esac
echo " "
fi

## There are some special cases where uname does not match the subpkg name
SavedMachname=$Machname
Machname=$TGMachname

section "checking subpackages"
    LastBom=""
    Subpkgs=""   
    for Subpkg in `pkginfo subpkgalias`
      do
        mrcbox=n
	if echo $Subpkg | grep -i ${Machnamerx='^m[0-9-]'} >/dev/null 2>&1
          then
            mrcbox=y
          fi
	if echo $Subpkg | grep -i ${Machnamerx='^rc[0-9-]'} >/dev/null 2>&1
          then
            mrcbox=y
          fi
	if echo $Subpkg | grep -i ${Machnamerx='^rs[0-9-]'} >/dev/null 2>&1
          then
            mrcbox=y
          fi
	if echo $mrcbox | grep y >/dev/null 2>&1
          then
	    if [ "$Subpkg" = "$Machname" -o "$Subpkg" = "${Machname}_dev" -o "$Installall" = "y" ]
            then
              if [ "$Installall" = "y" ]
              then
                CurBom=`pkginfo bomname $Subpkg`
              fi
              if [ "$Installall" != "y" -o "$LastBom" != "$CurBom" ]
	      then
                if [ "$InstDiskless" != "y" ]
                then
		  Subpkgs="$Subpkgs $Subpkg"
                fi
               fi
               LastBom="$CurBom"
             fi
	  else
            Subpkgs="$Subpkgs $Subpkg"
	  fi
      done

echo "The following subpackages may be installed:"
echo
for pkg in $Subpkgs
do
  tempid=`pkginfo subpkgid $pkg`
  echo "$pkg	-- $tempid"
done
sleep 2
AllSubpkgs=$Subpkgs

section "selecting subpackages"

#REAL WORK BEGINS HERE
Ans=n
		  
echo "You may select all of the above subpackages by answering \"y\" to the "
echo "following question.  If you answer \"n\" then you will be asked to  "
echo "select the optional subpackages you would like to have installed."
echo " "
ask "Install ALL subpackages" n y n
echo " "

# This subscript allows the installer to select the combination
# of subpackages to install. It places a list of subpackages to
# install in the variable Subpkgs
#

#
# If we're installing an os release, and running on the miniroot,
# then force the non-optional subpackages. Otherwise, ask about
# all non-minirootonly subpackages
# 

while [ "$Ans" = "n" ]
  do
    echo " "
    echo "When asked if you want to install a subpackage, please answer with "
    echo "one of the following: "
    echo " "
    echo "  y - Yes, you want to install the subpackage"
    echo "  n - No, you do NOT want to install the subpackage"
    echo "  l - List the contents of the subpackage and ask me again"
    echo " "
    LastBom=""
    Subpkgs=""   
    for Subpkg in $AllSubpkgs
      do
        mrcbox=n
	if echo $Subpkg | grep -i ${Machnamerx='^m[0-9-]'} >/dev/null 2>&1
          then
            mrcbox=y
          fi
	if echo $Subpkg | grep -i ${Machnamerx='^rc[0-9-]'} >/dev/null 2>&1
          then
            mrcbox=y
          fi
	if echo $Subpkg | grep -i ${Machnamerx='^rs[0-9-]'} >/dev/null 2>&1
          then
            mrcbox=y
          fi
	if echo $mrcbox | grep y >/dev/null 2>&1
        then
	  if [ "$InstDiskless" != "y" -a "$Subpkg" = "$Machname" -o "$Subpkg" = "${Machname}_dev" -o "$Installall" = "y" ]
	  then
            if [ "$Installall" = "y" ]
            then
              CurBom=`pkginfo bomname $Subpkg`
            fi
            if [ "$Installall" != "y" -o "$LastBom" != "$CurBom" ]
	    then
              case "$Onmini" in
                n) Ans=l
                   while [ "$Ans" = "l" ]
                   do
		     ask "Install subpackage $Subpkg" y l y n
                     case $Ans in
                       y) Subpkgs="$Subpkgs $Subpkg" ;;
                       l) echo " "
                          echo "Listing of subpackage $Subpkg:"
                          sleep 2
                          TheBom=`pkginfo bomname $Subpkg`
                          cat $Pkg/boms/$TheBom | sed "s/ .*//" | pg -n23 -p"Press [RETURN] for more,    q to quit: "
                          echo " "
                          ;;
                     esac
                   done
		   ;;
		y) case $Install in
                   update) Ans=l
                           while [ "$Ans" = "l" ]
                           do
			     ask "Install subpackage $Subpkg" y l y n
                             case $Ans in
				y) Subpkgs="$Subpkgs $Subpkg" ;;
                                l) echo " "
                                   echo "Listing of subpackage $Subpkg:"
                                   sleep 2
                            TheBom=`pkginfo bomname $Subpkg`
                            cat $Pkg/boms/$TheBom | sed "s/ .*//" | pg -n23 -p"Press [RETURN] for more,    q to quit: "
                            echo " "
                                   ;;
                             esac
                           done
			   ;;
                   scratch)
		     echo "Subpackage $Subpkg will be installed."
		     Subpkgs="$Subpkgs $Subpkg" 
		     ;;
		   esac
		 ;;
	       esac
            fi
            LastBom="$CurBom"
          fi
	else
             pkginfo optional $Subpkg 
            if pkginfo optional $Subpkg >/dev/null 2>$Verbose
              then
                Optional=y
              else
                Optional=n
              fi
            if pkginfo minirootonly $Subpkg >/dev/null 2>$Verbose
              then
                Mronly=y
              else
	        Mronly=n
              fi
            Action=none
            case "$Onmini" in
              y) case "$Optional" in
                   y) Action=ask ; Def=n ;;
                   n) Action=force ;;
                   esac ;;
              n) case "$Mronly" in
                   n) Action=ask ; Def="$Optional" ;;
                   esac ;;
              esac
	    if [ $Install = "update" ]
	    then
		Action=ask
		Def=n
	    fi
            case "$Action" in
               ask) Ans=l
                     while [ "$Ans" = "l" ]
                     do
		       ask "Install subpackage $Subpkg" $Def l y n
                       case $Ans in
                         y) Subpkgs="$Subpkgs $Subpkg" ;;
                         l) echo " "
                            echo "Listing of subpackage $Subpkg:"
                            sleep 2
                            TheBom=`pkginfo bomname $Subpkg`
                            cat $Pkg/boms/$TheBom | sed "s/ .*//" | pg -n23 -p"Press [RETURN] for more,    q to quit: "
                            echo " "
                            ;;
                       esac
                     done
                     ;;
             force) echo "Subpackage $Subpkg will be installed."
                    Subpkgs="$Subpkgs $Subpkg" ;;
             esac
	  fi
      done

    case "$Subpkgs" in
      "") ask "\nNo subpackages are selected. Quit install" y y n
          case "$Ans" in
	    y) exit 1 ;;
            esac
	  echo ""
	  ;;
       *) echo "\nSelected subpackages:
  $Subpkgs"
          ask "Is this what you want" y y n
	  case $Ans in
	    n) echo "" ;;
	    esac
	  ;;
       esac
  done

Machname=$SavedMachname
