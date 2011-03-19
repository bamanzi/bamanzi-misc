#!/bin/sh
# This script is written for Cygwin
# meant to install packages (i.e to extract files from tarballs(s)).
# And a list for files extracted would be generated in /etc/setup, 
# so that these packages could be uninstalled by removepkg.sh
# or Cygwin Setup (if the default installed.db was used when installing). 
#
# Ralgh Young  ralgh.young@gmail.com
#
# History:
#  .1 
#  .2 add -v|-verbose option
#  .3 fixed a bug in generating file list in /etc/setup/
#  .4 show package description, acts like Slackware 
#  .5 add -f|--force option
#  .6 move package description displaying to before extracting.
#  .7 added -g|--global-db, -l|--local-db option
#  .8 enabled post-install script support
#  .9 enabled alternative ROOT for install database
# .10 exec postinstall scripts after all packages unpacked
# .11 made localdb defaults to 0
#     bugfix: fail when package's path contains spaces
#     add usage for option '-l' and ROOT
# .12 try to locate setup.ini and show package description from it
#     bugfix: checking whether a package is installed should be before displaying package description


Usage()
{
    echo "Usage: `basename $0` [-h|--help]"
    echo "       `basename $0` [-f|--force|-s|--silent][-v|--verbose] pkgtarball1 [pkgtarball2...]"
    echo "           -s|--slient: suppress unnecessary messages (such as package description)"
    echo "           -v|--verbose:  show list of files installed by package"
    echo "           -f|--force:  force installation even if already installed"
    echo "           -l|--local:  use /etc/setup/local/ for storing installed.db"
    echo "You can use an alternative root to maintain another installed.db, e.g.:"
    echo "         ROOT=/opt/gnome installpkg.sh foobar.tar.bz2"
    echo "     Thus all info (installed.db, foobar.lst.gz) would be save to /usr/local/etc/setup "
    echo "     You should use the same ROOT when uninstalling package with removepkg.sh."
    echo "     Note: option '-l' couldn't be used together with this option"

}

find_desc_in_setup_ini()
{
    pkgname=$1
    setup_ini=$2
    awk -v pkg=$pkgname '
    BEGIN { found =0 }
    /^@/ { if ($2==pkg) found =1; else found=0}
    { if (found==1) { print $0 } }' $setup_ini
} 

if [ $# -eq 0 ]; then
    Usage
    exit 1    
fi

if [ x$1 = 'x-h' -o x$1 = '--help' ]; then
    Usage
    exit 0
fi

verbose=0
force=0
silent=0
localdb=0
# when ROOT specified, installed database would be $ROOT/etc/setup
[ -n "$ROOT" ] && localdb=0
while [ $# -gt 0 ]; do
    case $1 in 
     -v|--verbose)  verbose=1; shift;;
     -f|--force)   force=1;  shift;;
     -s|--silent)  silent=1;  shift;;
     -g|--global-db) localdb=0; shift;; 
     -l|--local-db)  localdb=1; shift;; 
     --)  shift;  break;; 
     -*) echo " '$1': unknown option"; exit 1;;
     [^-]*)  break;; 
    esac
done      

if [ $localdb -eq 1 ]; then
    installed_db_dir=/etc/setup/local
else
    installed_db_dir=$ROOT/etc/setup
fi
[ ! -d $installed_db_dir ] && mkdir -p $installed_db_dir
INSTALLED_DB=${installed_db_dir}/installed.db
    
pkgok=0
if [ ! -f $INSTALLED_DB ]; then
   touch $INSTALLED_DB
   echo "INSTALLED.DB 2" > $INSTALLED_DB
fi

# make backup of $ROOT/etc/setup/installed.db
cp -f $INSTALLED_DB ${INSTALLED_DB}.old

for pkgfile in $*; do
    # parse absolute path of pkgfile
    echo "$pkgfile" | grep -q '^/'
    if [ $? -ne 0 ]; then  # not absolute path
        pkgfile=`pwd`/$pkgfile
    fi
    if [ ! -f "$pkgfile" ]; then
        echo "error: file $pkgfile doesn't exist!"
        continue
    fi

    basefn=`basename "$pkgfile"`

    # all chars before first digit is pkgname
    # FIXME: version must be started with a digit
    #pkgname=`echo $basefn | sed -e "s/[_\-][0-9].*$//"`	
    pkgname=`echo $basefn | sed -e "s/\-[0-9].*$//"`	
    if [ x$pkgname = x ]; then
        echo "${basefn}: error: can\'t determine package name, skipped"
        continue
    fi

    test $verbose -eq 0 && echo "Installing $pkgname ......"    

    # check whether it has been installed
    grep -q "^$pkgname\ " $INSTALLED_DB
    if [ $? -eq 0 -a $force -ne 1 ]; then
        echo "    package $pkgname already installed, skipped."
        continue
    fi

    # show package info stored in *.desc
    #  if [ -f `dirname "$pkgfile"`/descript.ion ]; then
    pkgdir=`dirname "$pkgfile"`
    if [ $silent -eq 0 ]; then
	 # the Cygwin style package info
	 # TODO: to show ldesc in setup.hint when in verbose mode
	 if [ -f $pkgdir/setup.hint ]; then
	    #[ $verbose -eq 0 ] && echo " "
            #echo -n "    "$pkgname ": " 
            #grep -is "^sdesc" "$pkgdir/setup.hint" | cut -d":" -f 2-
            echo "============================================================="
	    cat $pkgdir/setup.hint
            echo "============================================================="
	 else
	    # try find setup.ini and search package description from it
	    echo $pkgfile | grep --quiet -s "release/"
	    if [ $? -eq 0 ]; then
		parentpath=$pkgfile
		parentdir=`basename $parentpath`
		while [ "$parentpath" != "/" -a "$parentdir" != 'release' ]
		do
		    parentpath=`dirname $parentpath`
		    parentdir=`basename $parentpath`
		done
		if [ "$parentdir" = "release" ]; then
		    setup_ini=`dirname ${parentpath}`/setup.ini
                    if [ -f $setup_ini ]; then
			echo "============================================================="
                        find_desc_in_setup_ini $pkgname $setup_ini #| grep -is "^sdesc" "$pkgdir/setup.hint" | cut -d":" -f 2-
			echo "============================================================="
		    fi
		fi
                    
            fi
	 fi
	 # the Slackware style package info
	 grep -is "^$pkgname: " "$pkgdir/*.desc" "$pkgdir/disk*" > /tmp/pkgdesc.tmp
         if [ $? -eq 0 ]; then
             test $verbose -eq 0 && echo " "
	     grep -is "^$pkgname: " "$pkgdir/*.desc" "$pkgdir/disk*" | cut -d' ' -f2-
             echo "============================================================="
         fi
    fi
    
        
    lstfile=$installed_db_dir/$pkgname.lst
    
    # find appropriate unpacker
    ext=`echo "$pkgfile" | sed -e "s/.*\.//"`
    case $ext in
        tgz|gz|TGZ|GZ) unpacker='--gzip' ;;
        bz2|BZ2|bzip2) unpacker='--bzip2' ;;
        z|Z) unpacker='--compress' ;;
        *) ;;
    esac  

    # extract files    
    #( cd /; tar tf "$pkgfile" $unpacker )  # for test
    [ $verbose -eq 1 ] && echo "    Unpacking files from $basefn ..."
    [ $verbose -eq 1 ] && echo "    Package list stored in $lstfile."
    if [ $verbose -eq 1 ]; then
        ( cd /; tar xvf "$pkgfile" $unpacker | tee $lstfile )
    else
        ( cd /; tar xvf "$pkgfile" $unpacker > $lstfile )
    fi
   
    # post-install
    if [ $? -eq 0 ]; then
	#grep --quiet etc/postinstall $lstfile
	#if [ $? -eq 0 ]; then
	    postscripts=`grep etc/postinstall $lstfile`
	    all_postinstall_scripts="$all_postinstall_scripts $postscripts"
	#fi
	    
    
	gzip $lstfile 
	# workaround: Cygwin Setup cannot recognize foo.tgz in install.db
	# but prefer to foo.tar.gz
	pkgfile=`echo "$pkgfile" | sed -e "s/tgz$/tar.gz/"`
	
	# register current package
	echo "$pkgname `basename "$pkgfile"` 0" >> $INSTALLED_DB

 	test $verbose -eq 0 && echo "    Done."
	pkgok=`expr $pkgok + 1`
   fi
done

# if more then one package installed successfully
if [ $pkgok -gt 0 ]; then
	    for s in $all_postinstall_scripts; do
		if [ -f /$s ]; then
		    echo -n "Executing post-install script /$s..."
		    (cd /; exec /$s ) && mv /$s /$s.done && echo "Done." || echo ""	
		fi
	    done
	#cp -f /tmp/installed.db.old $installed_db_dir/installed.db.old

	# cp -f $installed_db_dir/installed.db /tmp/installed.db.tosort
	# cat /tmp/installed.db.tosort | sort > $installed_db_dir/installed.db
	# rm -f /tmp/installed.db.tosort
fi
#rm -f /tmp/installed.db.old

# vim:sts=4
