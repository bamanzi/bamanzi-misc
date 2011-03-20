#!/bin/python

from sgmllib import SGMLParser
import htmlentitydefs


import os


class HTMLKeywordParser(SGMLParser):
    def __init__(self):
        SGMLParser.__init__(self)
        self.filename=""
        self.parseTitle = True  # whether to parse the <title> tag
        self.parseMetaTag = True  # whether to parse the <meta name="Keywords" content="keyword1,keyword2"/>
        self.parseExternalLink = False # whether to parse the <a> tag linked to external urls
									   
        
        self._inhref = False
        self._title = self.url = ""
        
    def reset(self):    
        self._inhref = False
        self._title = self.url = ""
        SGMLParser.reset(self)
    
    def start_meta(self, attrs):
        if not self.parseMetaTag: return
        iskeywords=False
        for attr in attrs:
            if attr[0].lower()=='name':
                iskeywords=True
            if iskeywords and (attr[0].lower()=='content'):
                keywords=attr[1].split(',')
                for keyword in keywords:
                    self.on_keyword(keyword, self.filename)
                    
    def start_title(self, attrs):
        self._inhref = True
 
    def end_title(self):
        if self._inhref and self._title:
            if self.parseTitle:
                self.on_keyword(self._title.replace('"', ""), self.filename)
            self._inhref=False
            self._title = self.url = ""
        
    def start_a(self, attrs):
        for attr in attrs:
            if attr[0].lower()=='href':
                self._inhref = True
                self._title = ""
                self.url = attr[1]
                break

    def end_a(self):
        if self._inhref and self._title and self.url:
            #if self.url[0:11]!="javascript:":
            if self.url[0]=="#":    # archors in current file
                url=self.filename + self.url
                self.on_keyword(self._title.replace('"', ""), url)
            elif self.parseExternalLink:  # target not in current file
                # we need to normpath ("dir1/dir2/../..")
                url=os.path.normpath(os.path.dirname(self.filename)+"/"+self.url)
                self.on_keyword(self._title.replace('"', ""), url)
            self._inhref = False
            self._title = self.url = ""

    def handle_data(self, text): 
        if self._inhref:
            self._title += text
    
    def handle_charref(self, ref):
        if self._inhref:
            self._title += "&#%(ref)s;" % locals()

    def handle_entityref(self, ref):        
        if self._inhref:
            self._title += "&%(ref)s" % locals()
            if htmlentitydefs.entitydefs.has_key(ref):
                self._title += ";"
            
    def on_keyword(self, text, url):
        pass


if __name__=='__main__':
    import sys
    from chmmaker import HHKWriter
    if len(sys.argv)==1:
        print "Usage: %s htmldirs..." % sys.argv[0]
        sys.exit()
    
    # FIXME: output filename hard-coded here
    hhk=HHKWriter('foo.hhk')
    hhk.print_header()

    exclude_names=["skip to content", "Test Page"]
    
    f = ""
    def my_on_keyword(text, url):
        title=text
        if title in exclude_names:
            return
        if url[0:11]=="javascript:":
            return
        if title[0:3].lower()=="the ":
            title = title[4:]
        elif title[0:1].lower()=="a ":
            title = title[2:] + ", " + "A"
        elif title[0:2].lower()=="an ":
            title = title[3:] + ", " + "An"
        
        target=url
        if url[0]=='#':
            target= f + url
        if title and target:
            hhk.add_keyword(title, target)

    parser=HTMLKeywordParser()
    parser.on_keyword = my_on_keyword
    import glob
    
    for d in sys.argv[1:]:
        #for f in glob.glob(d+'/*'):
        for root, dirs, files in os.walk(d):  # use os.walk so we can recursively process
            print "analyzing %s ..." % root,
            for fn in files:
                f=os.path.join(root, fn)
                if os.path.isdir(f): continue
                if not (os.path.splitext(f)[1] in [".htm", ".html"]): continue 
                #print "analyzing %s ..." % f
                print ".",
                fh=open(f, "r")
                contents=fh.read()
                fh.close()
                parser.filename=f
                try:
                    parser.feed(contents)
                except:
                    print "error in %s: %s " % (f, sys.exc_info()[1])
                #parser.feed(contents)
                parser.reset()
            print "done"  # root
       
    parser.close()
    hhk.print_footer()

# vim:sts=4 expandtab
 


