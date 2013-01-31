#!/usr/bin/python

# rbt2dot.py
# Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
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
transform the red black tree output result to dot script (graphviz)
usage: rbt2dot.py -o filename.dot "((. 1:R .) 2:B .)"
       R: red, B: black, BB: double-black, N: open branch, C: unknown color
       Color can be omit, so for normal BST it can be use like:
       rbt2dot.py -o filename.dot "((. 1 .) 2 .)"
"""

import sys
import string
import getopt

COLOR_DICT = {"R":"fillcolor=lightgray, fontcolor=black", 
              "B":"fillcolor=black, fontcolor=white",
              "BB":"fillcolor=black, fontcolor=white, peripheries=2", #BB: doubly black
              "C":"fillcolor=white, fontcolor=black", #C: unknow color
              "N":"color=white"} #N: this is not a node, but a sub stree

class Node:
    def __init__(self):
        self.key = ""
        self.color = "C" #default color
        self.left = self.right =None

def str_to_tree(ts):
    ts=ts.lstrip()
    x = Node()
    state=""
    while True:
        (c, ts)=(ts[0], ts[1:])
        if c== '.':
            return (None, ts)
        elif c =='(':
            (x.left, ts)=str_to_tree(ts)
            state = "parsing-key"
        elif c==' ':
            if state == "parsing-key":
                (x.key, ts) = parsing_field(ts)
                state = "parsing-color"
            elif state=="parsing-color":
                (x.right, ts)=str_to_tree(ts)
        elif c==')':
            return (x, ts)
        elif c==':':
            (x.color, ts) = parsing_field(ts)
        else:
            if state=="parsing-key":
                x.key=x.key+c
            elif state=="parsing-color":
                x.color=x.color+c
            else:
                print "[parsing error, state=", state, " c=", c,"]"
                sys.exit()

def parsing_field(ts):
    res = ""
    while ts!=[]:
        (c, ts) = (ts[0], ts[1:])
        if ":. ()".find(c)>=0:
            return (res, c+ts)
        res = res + c
    return (res, ts)

def leaf(node):
    return node.left is None and node.right is None

def define_connection(node, prefix):
    (left, leftstyle, right, rightstyle, res)=("", "\n", "", "\n", "")
    (mid, midstyle)=("nil" + prefix + "m" + node.key, "[style=invis];\n")

    if node.left is None:
        left = "nil" + prefix + "l" + node.key
        leftstyle="[style=invis];\n"
    else:
        left = prefix + "l" + node.left.key

    if node.right is None:
        right = "nil" + prefix + "r" + node.key
        rightstyle="[style=invis];\n"
    else:
        right = prefix + "r" + node.right.key

    res = res + "\t" +prefix + node.key + "->" + left + leftstyle
    res = res + "\t" +prefix + node.key + "->" + mid + midstyle
    res = res + "\t" +prefix + node.key + "->" + right + rightstyle
    res = res + "\t{rank=same " + left + "->" + mid + "->" + right + "[style=invis]}\n"
    return res

def define_node(node, prefix="a", parent=None):
    res="\t"
    if node is None:
        res = res + "nil" + prefix + parent.key + "[label=\"\", style=invis];\n"
    else:
        res = res + prefix + node.key + "[label=\"" +node.key + "\", style=filled, " + COLOR_DICT[node.color] + "];\n"
        if not leaf(node):
            res = res + define_node(node.left, prefix + "l", node) + define_node(node.right, prefix + "r", node)
            res = res + define_node(None, prefix + "m", node) #used for balancing, refer to [1]
            res = res + define_connection(node, prefix)
    return res

def tree_to_dot(t, prefix, filename):
    res="digraph G{\n" "\tnode[shape=circle]\n"
    res=res+define_node(t, prefix)
    res=res+"}"
    print res
    f=open(filename,'w')
    f.write(res)
    f.close()
    print "write to file ", filename, "OK."

def get_args(argv):
    try:
        prefix="a"
        opts, args = getopt.getopt(argv, "o:p:")
        for opt, arg in opts:
            if opt == "-o":
                filename=arg
            elif opt == "-p":
                prefix=arg
            else:
                usage()
        return (filename, prefix, args[0])
    except getopt.GetoptError:
        usage()

def usage():
    print __doc__
    sys.exit()

def main(argv):
    (filename, prefix, ts)=get_args(argv)
    (t, ts)=str_to_tree(ts)
    tree_to_dot(t, prefix, filename)

if __name__ == "__main__":
    main(sys.argv[1:])

#reference
#[1] http://www.graphviz.org/mywiki/FaqBalanceTree
