#!/usr/bin/python
"""Generate an index.html (and frame.html) from a HHC file, which
contains the content tree, so that you can browser the help tree with
a browser."""

import os.path

from hhcparser import HHCParser

HTML_TPL_HEADER = """<html>
<head>
   <title>%(title)s</title>
   <style>
     ul { margin-left: none }
     li { margin: 2px 20px }
   </style>
   <script>

   </script>
</head>
<body>
<ul>
"""

HTML_TPL_FOOTER = "</ul>
</html>"""

HTML_TPL_FOLDER_BEGIN = """<li class="icon%(icon)s"><a href="javascript:toggle('%(id)s')">
<img border="0" height="16" width="16" src="folder.gif">&nbsp;
<a href="%(url)s">%(title)s</a></li>
<UL>"""

HTML_TPL_FOLDER_END   = """</UL>"""

HTML_TPL_ITEM         = """<li class="icon%(icon)s"><img border="0" height="16" width="16" src="file.gif">&nbsp;
<a href="%(url)s">%(title)s</a></li>"""

def gen_index_html_from_hhc(hhcfilepath, title):
    hhcfile = file(hhcfilepath)

    hhc = HHCParser()
    for line in hhcfile:
        hhc.feed(line)
    hhcfile.close()

    
    def node_to_html(node):
        if len(node.children)==0:
            html.write(HTML_TPL_ITEM % node.data)
        else:
            if node.data:
                node.data['id'] = id(node.data)
                html.write(HTML_TPL_FOLDER_BEGIN % node.data)

            for child in node.children:
                node_to_html(child)

            if node.data:
                html.write(HTML_TPL_FOLDER_END % node.data)
    
    html = file("index.html", "w")
    html.write(HTML_TPL_HEADER % { 'title': title } )

    node_to_html(hhc.contents)

    html.write(HTML_TPL_FOOTER)


if __name__ == "__main__":
    def usage():
        arg0 = os.path.basename(sys.argv[0])
        sys.stderr.write("Usage: %s foo.hhc [title]" % arg0)

    if len(sys.argv)<2:
        usage()
        sys.exit(1)

    hhcfilepath = sys.argv[1]
    if not os.path.exists(hhcfilepath):
        sys.stderr.write("Error: file not exists: %s" % hhcfilepath)
        usage()
        sys.exit(2)

    title = sys.argv[2] if len(sys.argv)>2 else ""
    gen_index_html_from_hhc(hhcfilepath, title)
