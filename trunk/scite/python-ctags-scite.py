#!/usr/bin/python

"""

In your SciteUser.properties, configure like this:

command.name.41.*=List symbols (ctags)
command.mode.41.*=subsystem:console,savebefore:no
command.41.*=python $(SciteUserHome)/pyctags-scite.py list $(FileNameExt)

command.name.42.*=Find definition in current file (ctags)
command.mode.42.*=subsystem:console,savebefore:no
command.42.*=python $(SciteUserHome)/pyctags-scite.py find $(CurrentSelection) $(FileNameExt)

command.name.43.*=Find definition across files (ctags)
command.mode.43.*=subsystem:console,savebefore:no
command.43.*=python $(SciteUserHome)/pyctags-scite.py find $(CurrentSelection)

user.context.menu=\
||\
Block &Comment or Uncomment|IDM_BLOCK_COMMENT|\
Toggle &Bookmark|IDM_BOOKMARK_TOGGLE|\
||\
&Grep in this file|1145|\
&Ack in all files|1146|\
&Find definition (ctags)|1143|


"""

import ctags
from ctags import CTags, TagEntry

import os
import sys


def _gen_ctags(filename):
    """ Parse a single file and return ctags output. """
    
    tempdir = '/tmp'
    if os.environ.has_key('TEMP'):
        tempdir = os.environ['TEMP']
    tempfile = os.path.join(tempdir, "tags")
    os.system('ctags -f "%s" --fields=afmikKlnsStz "%s"' % (tempfile, filename))

    # Available file information keys:
    #  opened -  was the tag file successfully opened?
    #  error_number - errno value when 'opened' is false
    #  format - format of tag file (1 = original, 2 = extended)
    #  sort - how is the tag file sorted? 
    #  author - name of author of generating program (may be empy string)
    #  name - name of program (may be empy string)
    #  url - URL of distribution (may be empy string)
    #  version - program version (may be empty string)

    # print ctagsobj['name']
    # print ctagsobj['author']
    # print ctagsobj['format']    
    
    return CTags(tempfile)

def _find_ctags(path):
    """ Find a ctags file along the path. """
    if os.pathsep in path:
        path = os.path.abspath(path)
    parent, base = os.path.split(path)
    if os.path.exists(os.path.join(parent, 'tags')):
        return os.path.join(parent, 'tags')
    elif base == '':
        return None
    else:
        return _find_ctags(parent)

def list_tags(ctagsobj, sort=ctags.TAG_SORTED):
    """ List all symbols in a CTags object. """
    # Available sort type:
    #  TAG_UNSORTED, TAG_SORTED, TAG_FOLDSORTED

    # Note: use this only if you know how the tags file is sorted which is 
    # specified when you generate the tag file
    status = ctagsobj.setSortType(sort)

    entry = TagEntry()
    status = ctagsobj.first(entry)

    while status:
        # Available TagEntry keys:
        #  name - name of tag
        #  file - path of source file containing definition of tag
        #  pattern - pattern for locating source line (None if no pattern)
        #  lineNumber - line number in source file of tag definition (may be zero if not known)
        #  kind - kind of tag (none if not known)
        #  fileScope - is tag of file-limited scope?

        if entry['kind'] in ('function', 'member'):
            print "%(file)s:%(lineNumber)s: %(kind)s %(name)s\t%(pattern)s" % entry
        
        # Note: other keys will be assumed as an extension key and will 
        # return None if no such key is found

        # Step to the next tag in the file (replace entry if found)
        status = ctagsobj.next(entry)
        

def find_tag(ctagsobj, tag, options=ctags.TAG_IGNORECASE):
    # Finding a Tag Entry

    # Available options: 
    # TAG_PARTIALMATCH - begin with
    # TAG_FULLMATCH - full length matching
    # TAG_IGNORECASE - disable binary search
    # TAG_OBSERVECASE - case sensitive and allowed binary search to perform

    entry = TagEntry()
    status = ctagsobj.find(entry, tag, options)

    while status:
        print "%(file)s:%(lineNumber)s: %(kind)s %(name)s\t%(pattern)s" % entry

        # Find the next tag matching the name and options supplied to the 
        # most recent call to ctagsobj.find().  (replace the entry if found)
        status = ctagsobj.findNext(entry)

def check_args_or_usage(wanted, errmsg=""):
    if len(sys.argv) < wanted + 1:
        if errmsg:
            print errmsg
        else:
            print "Error: no enough args"
        print ""
        print "Usage: %s action [args]" % os.path.basename(sys.argv[0])
        print "    Ctags interaction for SciTE (exuberant_ctags needed)."
        print "available actions:"
        print "  list filename          List classs & functions of a file"
        print "  find symbol filename   Find definition of symbol in current file"
        print "  find symbol            Find definition of symbol in tags"        
        sys.exit(1)        

if __name__=='__main__':
    check_args_or_usage(1)
    
    action = sys.argv[1]
    if action == '--help':
        check_args_or_usage(99, "   ")
    elif action == 'list':
        # usage: $0 list sourcefile
        check_args_or_usage(2)

        ctagsobj = _gen_ctags(sys.argv[2])
        if not ctagsobj:
            print "Failed to generate tags file"
            sys.exit(1)

        list_tags(ctagsobj)
    elif action == 'find':
        # usage: $0 find symbol <sourcefile>
        check_args_or_usage(2)
        
        symbol = sys.argv[2]
        if len(sys.argv)>2:
            ctagsobj = _gen_ctags(sys.argv[3])
        else:
            ctagsobj = _find_ctags(os.getcwd())
            if not ctagsobj:
                print "File 'ctags' not found."

        find_tag(ctagsobj, symbol)
            
