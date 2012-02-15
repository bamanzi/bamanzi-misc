#!/bin/python

import sgmllib
import sys
class _TreeNode:
    def __init__(self, parent=None, data=None):
        self.children=[]
        self.parent=parent
        self.data=data

    def add_child(self, data=None):
        node = _TreeNode(self, data)
        self.children.append(node)
        return node

class HHCParser(sgmllib.SGMLParser):
    def reset(self):
        self.contents=None
        self._current_contents=None
        sgmllib.SGMLParser.reset(self)
    
    def start_object(self, attrs):
        self._title=None
        self._icon=None
        self._url=None
        self._isnew=False
        self._objtype=None
        if attrs[0][0]=="type":
            self._objtype=attrs[0][1]

    def end_object(self):
        #print self._title, " -> ", self._url
        if self._current_contents!=None and self._objtype=="text/sitemap":
            #print self._title, " -> ", self._url
            self._current_contents.add_child({ 'title': self._title,
                                               'url'  : self._url,
                                               'icon' : self._icon,
                                               'isnew': self._isnew})         

    def start_param(self, attrs):
        if len(attrs)>=2:
            # <param name="Name" value="Style:Technical, Yet Easy Reading">
            param=attrs[0][1]
            value=attrs[1][1]
            if param=="Name":
                self._title=value
            elif param=="Local":
                #<param name="Local" value="3.html">
                self._url=value
            elif param=="ImageNumber":
                self._icon=value
            elif param=="New":
                self._isnew=value

    def start_ul(self, attrs):
        if not self._current_contents:
            self.contents = _TreeNode()
            self._current_contents = self.contents
        else:
            if len(self._current_contents.children)>0:  
                self._current_contents = self._current_contents.children[-1]

    def end_ul(self):
        if self._current_contents:
            self._current_contents = self._current_contents.parent

    def dump(self, indent=0, root=None):
        if root==None:
            root=self.contents
        for node in root.children:
            if node.data!=None:
                title, url, icon, new=node.data
                print "  " * indent + "%s [%s]" % (title, url)
            if len(node.children)>0:
                self.dump(indent + 1, node)

if __name__=='__main__':
    f=file(sys.argv[1], "r")
    parser=HHCParser()

    lines=f.read()
    parser.feed(lines)
    f.close()

    parser.dump()


# vim:sts=4
