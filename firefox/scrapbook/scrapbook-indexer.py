#!/usr/bin/python

import sys
import os
import xml.parsers.expat

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
                    'id':attrs['NS1:id']
                    ,'type':attrs['NS1:type']
                    ,'title':attrs['NS1:title']
                    ,'source':attrs['NS1:source']
                    ,'chars':attrs['NS1:chars']
                    ,'icon':attrs['NS1:icon']
                    ,'comment':attrs['NS1:comment']
                    }


    def end_element(name):
        if "RDF:Seq" == name:
            if CF.IS_ROOT:
                CF.IS_ROOT = 0
            else:
                pass

    px = xml.parsers.expat.ParserCreate()
    px.StartElementHandler = start_element
    px.EndElementHandler = end_element
    px.Parse(file(sbrdffile).read(), 1)

    return CF.DICTRDF
        


if __name__=='__main__':
    import sys
    rdffile = os.path.abspath(sys.argv[1])
    sbdata = parse_scrapbook_rdf(rdffile)
                                 
    import pickle
    output = open('scraptools_%s.pkl' % os.path.dirname(rdffile)[-1] , 'wb')
    pickle.dump(sbdata, output)
    #output.close


