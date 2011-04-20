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
transform the trie to dot script (graphviz)
usage: 
trie2dot.py -o filename.dot "110, 0, 01, 01101"
trie2dot.py -o example.dot "001:y, 100:x, 101:z"
trie2dot.py -o foo.dot -t patricia "a, an, the, another, hello"
  o: output filename
  t: type, can be trie, patricia, suffix-trie, suffix-treex
"""

import sys
import string
import getopt
import trie
import patricia

def define_connection(n1, n2, edge):
    return "\t"+n1+"->"+n2+"[label=\""+edge+"\"]\n"

def define_node(node, prefix=""):
    res="\t"
    node_name = lambda x: "t"+x
    to_str = lambda x: "%s" %x
    node_attrib = "[label=\"\"];\n"
    if node.value is not None:
        node_attrib = "[label=\""+prefix + ":" + node.value + "\", shape=ellipse];\n"
    res += node_name(prefix)+node_attrib
    for k, v in sorted(node.children.items()):
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
        opts, args = getopt.getopt(argv, "o:t:")
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

def is_map(s):
    return not s.find(":") == -1

def to_map(s):
    m={}
    for elem in s.split(", "):
        (k, v) = elem.split(":")
        m[k]=v
    return m

def main(argv):
    (filename, type, ts)=get_args(argv)
    if type == "trie":
        if is_map(ts):
            t=trie.map_to_trie(to_map(ts))
        else:
            t=trie.list_to_trie(ts.split(", "))
        print trie.trie_to_str(t)
        trie_to_dot(t, filename)
    if type == "patricia":
        if is_map(ts):
            t=patricia.map_to_patricia(to_map(ts))
        else:
            t=patricia.list_to_patricia(ts.split(", "))
        print patricia.to_string(t)
        trie_to_dot(t, filename)

if __name__ == "__main__":
    main(sys.argv[1:])

#reference
#[1] http://www.graphviz.org/mywiki/FaqBalanceTree
