#!/usr/bin/python
"""
transform the trie to dot script (graphviz)
usage: 
trie2dot.py -o filename.dot -t "110, 0, 01, 01101"
trie2dot.py -o foo.dot -t "a, an, the, another, hello"
  o: output filename
  t: type, can be trie, patricia, suffix-trie, suffix-treex
"""

import sys
import string
import getopt
import trie

def define_connection(n1, n2, edge):
    return "\t"+n1+"->"+n2+"[label=\""+edge+"\"]\n"

def define_node(node, prefix=""):
    res="\t"
    node_name = lambda x: "t"+x
    to_str = lambda x: "%s" %x
    res += node_name(prefix)+"[label=\""+node.value+"\"];\n"
    for k, v in node.children.items():
        res += define_node(v, prefix+to_str(k))
        res += define_connection(node_name(prefix), node_name(prefix+to_str(k)), to_str(k))
    return res

def trie_to_dot(t, filename):
    res="digraph G{\n" "\tnode[shape=circle]\n"
    res=res+define_node(t)
    res=res+"}"
    print res
    f=open(filename,'w')
    f.write(res)
    f.close()
    print "write to file ", filename, "OK."

def get_args(argv):
    try:
        type="trie"
        opts, args = getopt.getopt(argv, "o:p:")
        for opt, arg in opts:
            if opt == "-o":
                filename=arg
            elif opt == "-t":
                type=arg
            else:
                usage()
        return (filename, type, args[0])
    except getopt.GetoptError:
        usage()

def usage():
    print __doc__
    sys.exit()

def main(argv):
    (filename, type, ts)=get_args(argv)
    if type == "trie":
        t=trie.list_to_trie(ts.split(", "))
        print trie.trie_to_str(t)
        trie_to_dot(t, filename)

if __name__ == "__main__":
    main(sys.argv[1:])

#reference
#[1] http://www.graphviz.org/mywiki/FaqBalanceTree
