#!/bin/python

import os, sys
import re
import glob
from copy import *

file_hhc = 0

class HHCWriter:
    """ This class is responsible for creating a .HHC file for your CHM Project. """
    def __init__(self, filename):
        self.hhcfile = file(filename, "w")
        self.indent_str   = "    "
        self.indent_level = 0
    
    def __del__(self):
        self.hhcfile.close()

    def print_header(self, params=None):
        """Print the header for a .HHC file.
        
        params: list of 2-tuples, specify the special params
                e.g [("Background", "0xffffff"), ("Foreground", "0x0")]"""
        self.hhcfile.write("""
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<HTML>
<HEAD>
<meta name="GENERATOR" content="Microsoft&reg; HTML Help Workshop 4.1">
<!-- Sitemap 1.0 -->
</HEAD><BODY>
<OBJECT type="text/site properties">
    <param name="ImageType" value="Folder">""")
        if params!=None:
            for name, value in params:
                self.hhcfile.write("""    <param name="%s" value="%s">\n""" % (name, value))
        self.hhcfile.write("""
</OBJECT>
<UL>
""")

    def print_footer(self):
        self.hhcfile.write(""" 
</UL>
</BODY></HTML>
""")

    def begin_section(self, name, url="", new=False, icon=None):
	""" begin_section name [url]: start a new section. 
	
	Note: If you want to start another section just after a TOPIC, you can use
	hhcfile.write("<UL>") (and "</ul>" to end it). """
        indent = self.indent_str * self.indent_level
        self.hhcfile.write( indent + "<LI><OBJECT type=\"text/sitemap\">\n")
        self.hhcfile.write( indent + "    <param name=\"Name\" value=\"%s\">\n" % name)
        if url!="":
            self.hhcfile.write( indent + "    <param name=\"Local\" value=\"%s\">\n" % url)
        if new:
            self.hhcfile.write( indent + "    <param name=\"New\" value=\"1\">\n")
        if icon!=None:
            self.hhcfile.write( indent + "    <param name=\"ImageNumber\" value=\"%s\">\n" % icon)
        self.hhcfile.write( indent + "    </OBJECT>\n")
        self.hhcfile.write( indent + "<UL>\n")
        self.indent_level = self.indent_level + 1

    def end_section(self):
        self.indent_level = self.indent_level - 1
        indent = self.indent_str * self.indent_level
        self.hhcfile.write( indent + "</UL>\n" )

    def add_topic(self, name, url, new=False, icon=None):
	# add_topic name url [new]
        indent = self.indent_str * self.indent_level
        self.hhcfile.write( indent + "<LI><OBJECT type=\"text/sitemap\">\n" )
        self.hhcfile.write( indent + "    <param name=\"Name\" value=\"%s\">\n" % name)
        self.hhcfile.write( indent + "    <param name=\"Local\" value=\"%s\">\n" % url)
        if new:    
            self.hhcfile.write( indent + "    <param name=\"New\" value=\"1\">\n")
        if icon!=None:
            self.hhcfile.write( indent + "    <param name=\"ImageNumber\" value=\"%s\">\n" % icon)   
        self.hhcfile.write( indent + "    </OBJECT>\n")


class HHKWriter:
    def __init__(self, filename):
        self.hhkfile = file(filename, "w")

    def __del__(self):
        self.hhkfile.close()

    def print_header(self): 
        self.hhkfile.write("""
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<HTML>
<HEAD>
<meta name="GENERATOR" content="Microsoft&reg; HTML Help Workshop 4.1">
<!-- Sitemap 1.0 -->
</HEAD><BODY>
<UL>
""")

    def print_footer(self):
        self.hhkfile.write("""
</UL>
</BODY></HTML>
""")
    
    def add_keyword(self, keyword, url, title=""):
	# TODO: to support multiple urls for one keyword
	# (add_keyword may called serveral times, each with a different url.
	#  Perhaps we should delay the writing until we got all the keywords)
        if title=="":
            # title = get_html_title(url)
            title=keyword
        self.hhkfile.write("""
    <LI> <OBJECT type="text/sitemap">
	<param name="Name" value="%s">
	<param name="Name" value="%s">
	<param name="Local" value="%s">
	</OBJECT>
""" % (keyword, title, url))

class HHPWriter:
    def __init__(self, filename):
        self.filename=filename
        self.chmfile=""
        self.hhcfile=""
        self.hhpfile=""
	self.hhkfile=""
        self.topic1=""
        self.title=""
        self.files = []  

    def add_files(self, files):
        if type(files) is list:
            self.files = self.files + files
        else:
            self.files.append(files)
    
    def flush(self):
        hhpfile = file(self.filename, "w")
        hhpfile.write("""[OPTIONS]
Compatibility=1.1 or later
Compiled file=%s
Contents file=%s
Index file=%s
Default Window=mainwin
Default topic=%s
Display compile progress=No
Full-text search=Yes
Language=0x409
Title=%s

""" % (self.chmfile, self.hhcfile, self.hhkfile, self.topic1, self.title))
        hhpfile.write("""
[WINDOWS]
mainwin=,"%s","%s","%s",,,,,,0x23520,,0x305e,,,,,,,,0 

""" % (self.hhcfile, self.hhkfile, self.topic1))
        hhpfile.write("[FILES]\n")
        for f in self.files:
            hhpfile.write(f + "\n")
        hhpfile.close()

    #def __setattr__(self, name, value):
    #    self.options[name]=value  #???


def get_html_title(filename):
    f=file(filename, "r")
    title=""
    line=f.readline()
    while line:
        #result=re.match(".*<title>([\<]*)\<+", line, re.IGNORECASE)
        result=re.search("<title>([^<]*)<", line, re.IGNORECASE)
        if result:
            title=result.group(1)
            line=f.readline()  # read another line, in case the title spans two lines
                               # FIXME: if the title spans more than two lines?
            result=re.match("([\>]*)</title>.*", line, re.IGNORECASE)
            if result:
                title=title+result.group(1)
            break
        line=f.readline()
    else:
        f.close()
    return title


def parse_keyword(title):
    keyword=title
    pos=title.find(" - ")
    if pos!=-1:
        keyword=title[0:pos]
    if title[0:4].lower()=="the ":
        keyword=keyword[4:] + ", the"
    return keyword
    
def generate_xfiles_chm_project(name):
    """An example: create an CHM project for all the X-Files transscripts,
    which downloaded from http://www.insidethex.com """
        
    #name="xfiles-transscripts"
    hhp=HHPWriter(name + ".hhp")
    hhp.chmfile=name+".chm"
    hhp.topic1="index.htm"
    hhp.title="Inside The X"
    
    hhc=HHCWriter(name + ".hhc")
    hhp.hhcfile=name+".hhc"
    params=[("Background", "0x0"), ("Foreground", "0xffffff")]
    hhc.print_header(params)
    
    hhk=HHKWriter(name + ".hhk")
    hhp.hhkfile=name+".hhk"
    hhk.print_header()
    
    
    for i in range(1, 10):
        #print "processing season %s" % i
        files=glob.glob("transcrp/scrp%s*.htm" %i) 
        if len(files)==0:
            break
    
        hhc.begin_section("Season %s" % i, icon=21)
        for f in files:
            hhp.add_files(f)
            title=get_html_title(f)
            hhc.add_topic(title, f, icon=27)
            hhk.add_keyword(parse_keyword(title), f, title)
    
        #hhc.end_section()
    
    hhc.print_footer()
    hhk.print_footer()
    hhp.flush()

if __name__ == '__main__':
    generate_xfiles_chm_project("xfiles-transscripts")
    try:
        os.execvp("hhw", ("xfiles-transscripts.hhp",))
    except:
        pass
    
# vim:sts=4
