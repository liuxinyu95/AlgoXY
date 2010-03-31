#!/usr/bin/python

# trie2dot.py, Convert tool, convert from Trie/Patricia to Dot script.
# Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
transform the suffix trie/tree to dot script (graphviz)
usage: 
st2dot.py -o filename.dot "banana"
st2dot.py -o example.dot -l "banana"
st2dot.py -o foo.dot -t trie -l "mississippi"
  o: output filename
  t: type, can be trie, tree, default is tree
  l: draw suffix links 
"""

import sys
import string
import getopt
import strie
import stree

def define_connection(n1, n2, edge):
    return "\t"+n1+"->"+n2+"[label=\""+edge+"\"]\n"

node_name = lambda x: "t"+x
node_attrib = "[label=\"\"];\n"

def define_trie_node(node, prefix=""):
    res="\t"
    to_str = lambda x: "%s" %x
    res += node_name(prefix)+node_attrib
    for k, v in sorted(node.children.items()):
        res += define_trie_node(v, prefix+to_str(k))
        res += define_connection(node_name(prefix), node_name(prefix+to_str(k)), to_str(k))
    return res

def define_tree_node(t, node, prefix=""):
    res="\t"
    to_str = lambda x: stree.substr(t.str, x)
    res += node_name(prefix)+node_attrib
    for c, (sref, tr) in sorted(node.children.items()):
        res += define_tree_node(t, tr, prefix+to_str(sref))
        res += define_connection(node_name(prefix), node_name(prefix+to_str(sref)), to_str(sref))
    return res

def to_dot(t, filename, type):
    res="digraph G{\n" "\tnode[shape=circle]\n"
    if type == "trie":
        res+=define_trie_node(t)
    else:
        res+=define_tree_node(t, t.root)
    res=res+"}"
    print res
    f=open(filename,'w')
    f.write(res)
    f.close()
    print "write to file ", filename, "OK."

def get_args(argv):
    try:
        type="tree"
        suffix_link=False
        opts, args = getopt.getopt(argv, "o:t:l") #o, t needs argument, while l does not
        for opt, arg in opts:
            if opt == "-o":
                filename=arg
            elif opt == "-t":
                type=arg
            elif opt == "-l":
                suffix_link = True
            else:
                usage()
        return (filename, type, suffix_link, args[0])
    except getopt.GetoptError:
        usage()

def usage():
    print __doc__
    sys.exit()

def main(argv):
    (filename, type, suffix_link, s)=get_args(argv)
    if type == "trie":
        t=strie.suffix_trie(s)
        print strie.to_str(t)
        to_dot(t, filename, type)
    if type == "tree":
        t=stree.suffix_tree(s)
        print stree.to_str(t)
        to_dot(t, filename, type)

if __name__ == "__main__":
    main(sys.argv[1:])

#reference
#[1] http://www.graphviz.org/mywiki/FaqBalanceTree
