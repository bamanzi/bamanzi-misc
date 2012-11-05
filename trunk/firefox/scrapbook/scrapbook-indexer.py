#!/usr/bin/python

import sys
import os

from bunch import Bunch

def parse_scrapbook_rdf(sbrdffile):
    """ parse rdf into a Bunch object, like this:
    { "ROOT": {'id':'',
               'li':[....] },
      "SEQ": {'seqXXX':['itemXXX', 'itemXXX'...],
              'seqXXX':['itemXXX', 'itemXXX'...],
              ...},
      "DESC": {'itemXX': {'id': '',
                        ,'type': ""    # ''|folder|separator|bookmark|mark
                        ,'icon': ''
                        ,'title': ''
                        ,'source': ''
                        ,'chars': ''
                        ,'comment': ''}
                ,,,
        }
    }
    """
    CF = Bunch(IS_ROOT = 0,
               IS_SEQ  = 0,
               IS_LI   = 0,
               IS_DESC = 0,
               DICTRDF = Bunch(ROOT = {},
                                SEQ = {},
                                DESC = {}
                                ),
               CRTID   = ""
               )
    
    #print pathto, CF.RDF% pathto, os.path.basename(pathto)
    def start_element(name, attrs):
        #print 'Start element:', name, attrs
        if "RDF:Seq" == name:
            CF.IS_SEQ = 1
            CF.IS_DESC = 0
            if "urn:scrapbook:root" == attrs['RDF:about']:
                #print 'ROOT element:', name, attrs
                CF.IS_ROOT = 1
                CF.DICTRDF['ROOT']['id'] = attrs['RDF:about'].split(":")[-1]
                CF.CRTID = attrs['RDF:about'].split(":")[-1]
                CF.DICTRDF['ROOT']['li'] = []
            else:
                CF.IS_ROOT = 0
                CF.CRTID = attrs['RDF:about'].split(":")[-1]
                CF.DICTRDF['SEQ'][CF.CRTID] = []
        else:
            CF.IS_SEQ = 0
            if "RDF:li" == name:
                CF.IS_DESC = 0
                CF.IS_LI = 1
                if CF.IS_ROOT:
                    CF.DICTRDF['ROOT']['li'].append(attrs['RDF:resource'].split(":")[-1])
                else:
                    CF.DICTRDF['SEQ'][CF.CRTID].append(attrs['RDF:resource'].split(":")[-1])
            elif "RDF:Description" == name:
                CF.IS_DESC = 1
                CF.IS_LI = 0
                CF.CRTID = attrs['RDF:about'].split(":")[-1]
                CF.DICTRDF['DESC'][CF.CRTID] = {
                    'id'      :attrs['NS1:id'],
                    'type'    :attrs['NS1:type'],
                    'title'   :attrs['NS1:title'],
                    'source'  :attrs['NS1:source'],
                    'chars'   :attrs['NS1:chars'],
                    'icon'    :attrs['NS1:icon'],
                    'comment' :attrs['NS1:comment']
                    }


    def end_element(name):
        if "RDF:Seq" == name:
            if CF.IS_ROOT:
                CF.IS_ROOT = 0
            else:
                pass

    import xml.parsers.expat

    px = xml.parsers.expat.ParserCreate()
    px.StartElementHandler = start_element
    px.EndElementHandler = end_element
    px.Parse(file(sbrdffile).read(), 1)

    return CF.DICTRDF

def scrapbook_to_html(sbdata, output):
    HTML_TMPL_HEADER = """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html;charset=UTF-8">
	<meta http-equiv="Content-Style-Type" content="text/css">
	<meta http-equiv="Content-Script-Type" content="text/javascript">
	<title>ScrapBook</title>
	<link rel="stylesheet" type="text/css" href="./output.css" media="all">
<style>
 body { font:11pt Corbel, Arial }
 table { font:9pt Corbel, Arial; background-color:#ffffdd; margin:5px }
 td { vertical-align:top }
 .hide { display:none; }
 .show { }
 .info, .info img { border:solid 1px #bbbbaa }
 .icon { width:16px; height:16px; border:none; padding-right:5px; vertical-align:middle }

li { white-space: nowrap; }
a.marked   { font-weight: bold; }
a.combine  { color: blue; }
a.bookmark { color: limegreen; }
</style>
	<script type="text/javascript" language="JavaScript">
<!--
var attach = function(o, e, f)
{
 if (o.attachEvent) o.attachEvent('on'+e, f);
 else if (o.addEventListener) o.addEventListener(e, f, false);
};
attach(window, 'load', function() {
 var x;
 var a=document.links;
 for (var i=0; i<a.length; i++) {
  var b=a[i].parentNode.childNodes;
  for (var j=0; j<b.length; j++) {
   if (b[j].className=="hide") {
    a[i].x=b[j];
    attach(a[i], 'mouseover',function(o) {
     if (x) x.className="hide";
     if (o.target) x=o.target;
     else if (o.srcElement) x=o.srcElement;
     if (x) {
      if (x.tagName.toLowerCase()=="img") x=x.parentNode.x;
      else x=x.x;
      if (x) x.className="show";
     }
    });
   }
  }
 }
});

	function toggle(aID) {
		var listElt = document.getElementById(aID);
		listElt.style.display = ( listElt.style.display == "none" ) ? "block" : "none";
	}
	function toggleAll(willOpen) {
		var ulElems = document.getElementsByTagName("UL");
		for ( var i = 1; i < ulElems.length; i++ ) {
			ulElems[i].style.display = willOpen ? "block" : "none";
		}
	}
	//--></script>
</head>

<body onload="toggleAll(false);">
<ul id="folder-root">
"""
    HTML_TMPL_FOOTER       = """
</ul>
</body></html>"""
    HTML_TMPL_BEGIN_FOLDER = """<li class="depth1">
<a class="folder" href="javascript:toggle('folder-%(id)s');"><img src="./tree/folder.png" width="16" height="16" alt="">%(title)s</a>
<ul id="folder-%(id)s">
"""
    HTML_TMPL_END_FOLDER   = "</ul>\n"
    HTML_TMPL_ITEM         = """<li class="depth2"><a href="data/%(id)s/index.html" target="main" class="">
<img src="./tree/treeitem.png" width="16" height="16" alt="">%(title)s</a>
  <div class="hide">
    <table class="info">
      <tr><td>Folder:</td><td><a href="data/%(id)s" target="main">data/%(id)s</a></td></tr>
      <tr><td>Source:</td><td><a href=%(source)s"   target="main">%(source)s</a></td></tr>
    </table>
  </div
</li>
"""
    
    def item_to_html(itemid):
        itemdata = sbdata.DESC[itemid]
        if not itemdata:
            sys.stderr.write("ERROR: description for item %s not exist.\n" % itemid)
            return

        type = itemdata["type"]
            
        if type=="folder":
            if not itemdata:
                sys.stderr.write("ERROR: itemdata is null. id=%s\n" % itemid)
            else:
                try:
                    output.write((HTML_TMPL_BEGIN_FOLDER % itemdata).encode('utf-8'))
                except:
                    result="error in folder %s\n" % itemid
                    for key in itemdata.keys():
                        result = result + "   %s: %s\n" %(key, itemdata[key].decode('UTF-8'))
                    sys.stderr.write(result)

                seq = sbdata.SEQ[itemid]
                if not seq:
                    sys.stderr.write("ERROR: seq for item %s not exist.\n" % itemid)
                else:
                    for item in seq:
                        item_to_html(item)
                    output.write((HTML_TMPL_END_FOLDER % itemdata).encode('utf-8'))
        else:  #site, bookmark, marked,
            try:
                output.write((HTML_TMPL_ITEM % itemdata).encode('UTF-8'))
            except UnicodeDecodeError as ex:
                sys.stderr.write("error in item %s:\n" % itemid)
                sys.stderr.write("  %s\n" % ex)

    output.write(HTML_TMPL_HEADER)
    for item in sbdata.ROOT["li"]:
        item_to_html(item)
    output.write(HTML_TMPL_FOOTER)


if __name__=='__main__':
    import sys
    #sys.setdefaultencoding('utf-8')
    rdffile = os.path.abspath(sys.argv[1])
    sbdata = parse_scrapbook_rdf(rdffile)
                                 
    # import pickle
    # output = open('scraptools_%s.pkl' % os.path.dirname(rdffile)[-1] , 'wb')
    # pickle.dump(sbdata, output)
    #output.close

    output = open('index.html', 'w')
    scrapbook_to_html(sbdata, output)


