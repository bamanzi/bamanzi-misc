#!/bin/sh


#History:
#  .1
#  .2 add -v|--verbose option to show the files currently deleting
#  .3 use 'tac' to reverse the order of removing files/folders
#  .8 enabled pre-remove script support
#  .9 enabled alternative ROOT for install database
# .11 made localdb defaults to 0
#     add usage info for option '-l' and ROOT
#     some other minor modification 
# .12 bug fixed: "foobar" removed from installed.db while uninstalling "foo".

Usage()
{
    echo "Usage: `basename $0` [-h|--help]"
    echo "       `basename $0` [-v|--verbose] pkg1name [pkg2name] ..."
    echo "           -v|--verbose:  show list of files installed by package"
    echo "           -l|--local:    use local INSTALLED_DB (/etc/setup/local/installed.db) "      
    echo "You can use ROOT environment to specify an alternative root to find installed.db,"
    echo "	  ROOT=/opt/gnome removepkg.sh foobar"
    echo "    Thus /opt/gnome/etc/setup would be searched for installed and package list file."
    echo "    You should use this option if you used it for installpkg.sh"
    echo "    Note: option '-l' couldn't be used together with this option."
}

if [ $# -eq 0 ]; then
    Usage
    exit 1
fi

verbose=0
force=0    #unused at now
localdb=0
# when ROOT specified, installed database would be $ROOT/etc/setup
[ -n "$ROOT" ] && localdb=0
while [ $# -gt 0 ]; do
    case $1 in 
     -v|--verbose)  verbose=1; shift;;
     -h|--help)  Usage; exit 0;;
     -f|--force)   force=1;  shift;;
     -g|--global)  localdb=0; shift;;
     -l|--local)   localdb=1; shift;;
     --)  shift;  break;; 
     -*) echo " '$1': unknown option"; exit 1;;
     [^-]*)  break;; 
    esac
done  

if [ $# -eq 0 ]; then
    Usage
    exit 1
fi

installed_db_dir="/etc/setup"
if [ $localdb -eq 1 ]; then
	installed_db_dir="/etc/setup/local"
else
	installed_db_dir="$ROOT/etc/setup"
fi
INSTALLED_DB="$installed_db_dir/installed.db"

# make backup
cp -f $INSTALLED_DB ${INSTALLED_DB}.old
INSTALLED_DBtmp1="/tmp/`basename $INSTALLED_DB`.tmp1"
INSTALLED_DBtmp2="/tmp/`basename $INSTALLED_DB`.tmp2"
cp -f $INSTALLED_DB $INSTALLED_DBtmp1
cd /

# for each package
for pkg in $*; do
    # get file lists
    lstfile=$installed_db_dir/$pkg.lst.gz
    if [ ! -f $lstfile ]; then
	test $verbose -eq 1 && echo "Can't find log file for package '$pkg', maybe it's not installed or wrong ROOT?"
	echo "    Log file not found (or perhaps wrong ROOT?). skipped"
	continue
    fi

    # handle preremove scripts
    # get postinstall script
    #zgrep --quiet etc/preremove $lstfile
    #if [ $? -eq 0 ]; then
	prescripts=`zgrep etc/preremove $lstfile`
	for s in $prescripts; do
	    if [ -f "/$s" ]; then
		echo -n "    Executine pre-remove scrpit /$s..."
		(cd /; exec /$s) && mv $s $s.done && echo "Done." || echo ""
	    fi
        done
    #fi


    # now, begin to remove files
    files=`zcat $lstfile | tac`
    for f in $files ; do 
	if [ -f $f ]; then
		test $verbose -eq 1 && echo "    Removing file $f"
		rm $f
		test $? -ne 0 && echo "       Failed to remove file $f"                
        elif [ -d $f ]; then
		test $verbose -eq 1 && echo "    Removing directory $f"
		rmdir --ignore-fail-on-non-empty -p $f
	fi
    done
    cp -f $INSTALLED_DBtmp1 $INSTALLED_DBtmp2 
    sed -e "/^$pkg /d" $INSTALLED_DBtmp2 > $INSTALLED_DBtmp1 

    [ $verbose -eq 1 ] && echo "    Removing file list $lstfile"
    rm $lstfile
done

# 
cp -f $INSTALLED_DBtmp1 $INSTALLED_DB

# vim:sts=4

