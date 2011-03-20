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
	self.current_contents=None
	sgmllib.SGMLParser.reset(self)
    
    def start_object(self, attrs):
	self.title=None
	self.icon=None
	self.url=None
	self.new=False
	self.objecttype=None
	if attrs[0][0]=="type":
	   self.objecttype=attrs[0][1]

    def end_object(self):
	#print self.title, " -> ", self.url
    	if self.current_contents!=None and self.objecttype=="text/sitemap":
	    #print self.title, " -> ", self.url
	    self.current_contents.add_child((self.title, self.url, self.icon, self.new))	

    def start_param(self, attrs):
	if len(attrs)>=2:
	    # <param name="Name" value="Style:Technical, Yet Easy Reading">
	    param=attrs[0][1]
	    value=attrs[1][1]
	    if param=="Name":
		self.title=value
	    elif param=="Local":
		#<param name="Local" value="3.html">
		self.url=value
	    elif param=="ImageNumber":
		self.icon=value
	    elif param=="New":
		self.icon=value

    def start_ul(self, attrs):
    	if not self.current_contents:
    	    self.contents = _TreeNode()
    	    self.current_contents = self.contents
    	else:
	    if len(self.current_contents.children)>1:	
		self.current_contents = self.current_contents.children[-1]

    def end_ul(self):
	if self.current_contents:
	    self.current_contents = self.current_contents.parent

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
