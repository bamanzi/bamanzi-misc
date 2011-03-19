#!/bin/sh
# Convert DevHelp bookname into a CHM file. V2.0
# Made for Cygwin, as CyGNOME DevHelp program runs too slowly
# But you can use it to generate .hhp, .hhc and .hhk file in other system.
# 
# Some assumption:
#     The book name and the directory name are the same, 
#     i.e, all html files of foo.devhelp are in directory /some/where/foo/.
#
#     File pathes in .hhk, .hhc and .hhk are relative path to the parent of 
#     the directory where html files are kept.
#     (i.e, reletive to /usr/share/devhelp/books in most cases).
#
#     Now v2.x supports building one CHM file from multiple DevHelp scroll.
#     For example: to build a CHM for all GTK-2.x books
#     $ devhelp2chm -c -d /usr/share/gtk-doc/html -v -p GTK-2.x -t "GTK-2.x DevHelp" \
#       /usr/share/gtk-doc/html/gdk-pixbuf/gdk-pixbuf.devhelp \
#       /usr/share/gtk-doc/html/gdk/gdk.devhelp \
#       /usr/share/gtk-doc/html/gtk/gtk.devhelp
#     (-c tells devhelp2chm to copy all files to current directory
#     so that they could be compiled by HHC/HHW.)
#
# Requirements:
#     xsltproc (in package libxslt2), to translate .devhelp file
#     M$ HTMLHelp Workshop (or hhc.exe only), to compile CHM project
#
# TO-DO:
#     Multi-book support (into one project)
#
# Changes
#     Mar 20, 2004
#       V2.0 Enabled multi-book support. 
#       (It would a root node for each book. If you don't want this for a single book,
#        use old versions)
#     Mar 17, 2004
#       Added option: --copy-files, --auto-build, --start-workshop
#	Embeded devhelp2hhc.xsl and devhelp2hhc.xsl
#     Mar 16, 2004 
#       First version
#
# Author
#     Ralgh Young (ralgh_young@yahoo.com)



Usage() 
{
    echo "Usage: $0 [OPTIONS] foo.devhelp ..."
    echo "     convert DevHelp book into a CHM file"
    echo "     -h|--help            Show this message"
    echo "     -p|--project         Project name (used in multi-book mode)"
    echo "     -t|--title           Project title"
    echo "     -d|--gtk-doc-dir     gtk-doc directory, ususally /usr/share/gtk-doc/html"
    echo "     -c|--copy-files      copy whole book to current directory    " 
    echo "     -b|--auto-build      start build when project file generated (hhc.exe needed)"
    echo "     -w|--start-workshop  use HTMLHelp workshop to open project file when done"
    echo "     -t|--default-topic   set default topic (default is book[0]/index.html)"
    echo "     e.g $0  /usr/share/gtk-doc/html /usr/share/gtk-doc/html/glib/glib.devhelp"
}

# e.g
#  gtk_doc_dir = /usr/share/devhelp/books/gdkpixbuf    
#  bookname=gdkpixbuf   # basename of gdkpixbuf.devhelp
  
verbose=0
auto_build=0
need_copy_files=0
start_workshop=0
gtk_doc_dir="./"
prjname=""
default_topic=""
title=""
#FIXME: use getopt instead
while [ $# -gt 0 ]; do
    case $1 in
     -h|--help)     Usage; exit 1;;
     -v|--verbose)          verbose=1; shift;;
     -p|--project)          prjname=$2; shift 2;;
     -t|--title)            title=$2; shift 2;;
     -d|--gtk-doc-dir)      gtk_doc_dir=$2;    shift 2;;
     -c|--copy-files)       need_copy_files=1; shift;;
     -b|--auto-build)       auto_build=1;      shift;;
     -t|--default-topic)    default_topic=$1;  shift;;
     -w|--start-workshop)   start_workshop=1;  shift;;  
     --)  shift;  break;;
     -*) echo " '$1': unknown option"; exit 1;;
     [^-]*)  break;;
    esac
done

if [ $# -eq 0 ]; then
    Usage
    exit 1
fi

[ x$prjname = "x" ] &&  prjname=`basename $1 .devhelp`

# generating book-list
echo '<?xml version="1.0" encoding="UTF-8"?>' >  ${prjname}-allbooks.xml
echo '<allbooks>'                             >> ${prjname}-allbooks.xml
for f in $*; do
     b="$f"
     
     grep --quiet xmlns "$f"
     if [ $? -eq 0 ]; then
         # remove xmlns declearation
         b=`basename $f`
         sed 's/xmlns="[^"]*"//' $f > $b.tmp
         b=$b.tmp
     fi

     echo "   <book href=\"$b\"/>"              >> ${prjname}-allbooks.xml
done     
echo '</allbooks>'                            >> ${prjname}-allbooks.xml 


# ============ Embedded devhelp2hhc.xsl =========================
cat >devhelp2hhc.xsl << EOF_DEVHELP2CHM_XSL
<xsl:stylesheet version='1.0'
		xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
		xmlns='http://www.w3.org/TR/REC-html40'>
 <!-- autogenerated by devhelp2chm.sh
      don't modify it manually, modify devhelp2chm.sh instead -->
 <xsl:output method="html" version="3.2" omit-xml-declaration="yes"/>
 
   <xsl:template match="allbooks">
    <HTML>
    <HEAD>
       <meta name="GENERATOR" content="DevHelp2CHM"/>
    &lt;!-- Sitemap 1.0 --&gt;
    </HEAD>
    <BODY>
      <UL>
      <xsl:for-each select="book">
         <xsl:variable name="book" select="document(@href)"/>
         <xsl:apply-templates select="\$book/book"/>
      </xsl:for-each>
       </UL>          
    </BODY>
    </HTML>      
   </xsl:template>
 
 <xsl:template match="/book">
         <LI>
          <OBJECT type="text/sitemap">
   <!--	e.g  <param name="Name" value="The GdkPixbuf Structure ">
	  <param name="Local" value="gdkpixbuf\gdk-pixbuf-gdk-pixbuf.html"> -->     
	  <xsl:text disable-output-escaping="yes">
	  &lt;param name="Name" value="</xsl:text>
	  <xsl:value-of select="/book/@title"/>
	  <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>
	  
	  <xsl:text disable-output-escaping="yes">	  
	  &lt;param name="Local" value="</xsl:text>
	  <xsl:value-of select="/book/@name"/>/<xsl:value-of select="/book/@link"/>          
	  <xsl:text disable-output-escaping="yes">"&gt;
	  </xsl:text>
	  </OBJECT>	  
        </LI>
        <UL>
	
        <xsl:apply-templates select="/book/chapters"/>

        </UL>
 </xsl:template>

 <xsl:template match="chapters">
      <xsl:for-each select="sub"> 
          <xsl:apply-templates select="."/>
      </xsl:for-each>
 </xsl:template>

 <xsl:template match="sub">
      <LI>
          <OBJECT type="text/sitemap">
<!--	  <param name="Name" value="The GdkPixbuf Structure ">
	  <param name="Local" value="gdkpixbuf\gdk-pixbuf-gdk-pixbuf.html"> -->     
	  <xsl:text disable-output-escaping="yes">
	  &lt;param name="Name" value="</xsl:text>
        <xsl:choose>
         <xsl:when test="string-length(@name)>0">
	  <xsl:value-of select="@name"/>
	 </xsl:when>
	 <xsl:otherwise>Index</xsl:otherwise>
	</xsl:choose>
	  <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>
	  
	  <xsl:text disable-output-escaping="yes">
	  &lt;param name="Local" value="</xsl:text>
	  <xsl:value-of select="/book/@name"/>/<xsl:value-of select="@link"/>          
	  <xsl:text disable-output-escaping="yes">"&gt;
	  </xsl:text>
	  </OBJECT>	  
      </LI>
      <xsl:if test="count(sub)>0">
          <UL>
          	<xsl:apply-templates select="sub"/>	
          </UL>
      </xsl:if>   
 </xsl:template>

</xsl:stylesheet>
EOF_DEVHELP2CHM_XSL

# ================== Embed devhelp2hhk.xsl  ==============
cat > devhelp2hhk.xsl << EOF_DEVHELP2HHK_XSL
<xsl:stylesheet version='1.0'
		xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
		xmlns='http://www.w3.org/TR/REC-html40'>
 <!-- autogenerated by devhelp2chm.sh
      don't modify it manually, modify devhelp2chm.sh instead -->
 <xsl:output method="html" version="3.2" omit-xml-declaration="yes"/>

 <xsl:template match="allbooks">
    <HTML>
    <HEAD>
       
    <!-- Sitemap 1.0 -->
    </HEAD>
    <BODY>
      <UL>      
      <xsl:for-each select="book">
         <xsl:variable name="book" select="document(@href)"/>
         <xsl:apply-templates select="\$book/book/functions"/>
      </xsl:for-each>
       </UL>          
    </BODY>
    </HTML>
 </xsl:template>

 <xsl:template match="functions">
      <xsl:for-each select="function"> 
          <xsl:apply-templates select="."/>
      </xsl:for-each>
 </xsl:template>

 <xsl:template match="function">
      <LI>
          <OBJECT type="text/sitemap">
<!-- e.g  <param name="Name" value="g_tree_insert">
          <param name="Name" value="g_tree_insert">
	  <param name="Local" value="glib-balanced-binary-trees.html#G-TREE-INSERT"> -->     
	  <xsl:text disable-output-escaping="yes">
	  &lt;param name="Name" value="</xsl:text>
	  <xsl:value-of select="@name"/>
	  <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>
	  
	  <xsl:text disable-output-escaping="yes">
	  &lt;param name="Name" value="</xsl:text>
	  <xsl:value-of select="@name"/>
	  <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>

	  <xsl:text disable-output-escaping="yes">
	  &lt;param name="Local" value="</xsl:text>
	  <xsl:value-of select="/book/@name"/>/<xsl:value-of select="@link"/>          
	  <xsl:text disable-output-escaping="yes">"&gt;
	  </xsl:text>
	  </OBJECT>	  
      </LI>
 </xsl:template>

</xsl:stylesheet>
EOF_DEVHELP2HHK_XSL

# generate the content and index file
test $verbose -eq 1 && echo "Generating content file ${prjname}.hhc"
xsltproc devhelp2hhc.xsl ${prjname}-allbooks.xml > ${prjname}.hhc
test $verbose -eq 1 && echo "Generating index file ${prjname}.hhk"
xsltproc devhelp2hhk.xsl ${prjname}-allbooks.xml > ${prjname}.hhk

# now the project file
test $verbose -eq 1 && echo "Generating project file ${bookname}.hhp"

#title=`grep "book title" "$filename" | cut -d"\"" -f2`
[ "x$title" = "x" ] && title="$prjname DevHelp Books [Made by DevHelp2CHM ]"
[ "x$default-topic" = "x" ] &&	default_topic=`basename $1 .devhelp`/index.html

cat > ${prjname}.hhp <<EOF
[OPTIONS]
Compatibility=1.1 or later
Compiled file=${prjname}.chm
Contents file=${prjname}.hhc
Index file=${prjname}.hhk
Default topic=$default_topic
Display compile progress=No
Full-text search=Yes
Language=0x409
Title=$title

[FILES]
EOF

for book in $*; do    
  if [ ! -f $book ]; then
     echo "File not exists: $filename "
     continue
  fi   
  bookname=`basename $book .devhelp`
  
  [ $verbose -eq 1 ] && echo "    Added files for book ${bookname} into ${bookname}.hhp"
  files=`cd $gtk_doc_dir; find ${bookname}/ -type f -name *.html; find ${bookname} -type f -name *.htm`
  for f in $files; do
      echo $f >> ${prjname}.hhp
  done
    
  if [ $need_copy_files -eq 1 -a $gtk_doc_dir != "./" ]; then
      test $verbose -eq 1 && echo "Copy whole book to ./${bookname}"
      cp -Ru ${gtk_doc_dir}/${bookname} $bookname
  fi
  
  [ -f ${bookname}.devhelp.tmp ] && rm -f ${bookname}.devhelp.tmp
done

# start building
# The following are Windows specific, you need cygstart in cygutils
if [ $auto_build -eq 1 ]; then
  test $verbose -eq 1 && echo -n "Start build ${bookname}.chm"
  cygstart hhc ${bookname}.hhp    
elif [ $start_workshop -eq 1 ]; then
  test $verbose -eq 1 && echo "Open project ${bookname}.hhp with HTMLHelp Workshop"
  cygstart hhw ${bookname}.hhp
  if [ $? -ne 0 ]; then
  	cygstart ${bookname}.hhp
  fi
fi
