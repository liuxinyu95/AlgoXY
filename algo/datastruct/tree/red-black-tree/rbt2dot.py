#!/usr/bin/python
"""
transform the red black tree output result to dot script (graphviz)
usage: rbt2dot.py -o filename.dot "((. 1:R .) 2:B .)"
"""

import sys
import string
import getopt

class Node:
    def __init__(self):
        self.key = ""
        self.color = "";
        self.left = self.right =None

def str_to_tree(ts):
    ts=ts.lstrip()
    x = Node()
    states=["parsing-key", "parsing-color"]
    state=""
    while True:
        c=ts[0]
        ts=ts[1:]
        if c== '.':
            return (None, ts)
        elif c =='(':
            (x.left, ts)=str_to_tree(ts)
            state = "parsing-key"
        elif c==' ':
            if state=="parsing-color":
                (x.right, ts)=str_to_tree(ts)
        elif c==')':
            return (x, ts)
        elif c==':':
            state="parsing-color"
        else:
            if state=="parsing-key":
                x.key=x.key+c
            elif state=="parsing-color":
                x.color=x.color+c
            else:
                print "[parsing error, state=", state, " c=", c,"]"
                sys.exit()

def define_connection(node, prefix):
    (left, leftstyle, right, rightstyle, res)=("", "\n", "", "\n", "")
    (mid, midstyle)=("nil" + prefix+node.key+"m", "[style=invis];\n")
    if node.left is None:
        left="nil"+prefix+node.key+"l"
        leftstyle="[style=invis];\n"
    else:
        left=prefix+node.left.key
    if node.right is None:
        right="nil"+prefix+node.key+"r"
        rightstyle="[style=invis];\n"
    else:
        right=prefix+node.right.key
    res=res+"\t"+prefix+node.key + "->" + left + leftstyle
    res=res+"\t"+prefix+node.key + "->" + mid + midstyle
    res=res+"\t"+prefix+node.key + "->" + right + rightstyle
    res=res+"\t{rank=same "+left+"->"+mid+"->"+right+"[style=invis]}\n"
    return res

def define_node(node, prefix="a", parent=None, postfix=""):
    res="\t"
    if node is None:
        res=res+"nil"+prefix+parent.key+postfix+"[label=\"\", style=invis];\n"
    else:
        color_dict = {"R":"fillcolor=lightgray, fontcolor=black", 
                      "B":"fillcolor=black, fontcolor=white"}
        res=res+prefix+node.key+"[label=\""+node.key+"\", style=filled, "+color_dict[node.color]+"];\n"
        res=res+define_node(node.left, prefix, node, "l")+define_node(node.right, prefix, node, "r")
        res=res+define_node(None, prefix, node, "m") #used for balancing, refer to [1]
        res=res+define_connection(node, prefix)
    return res

def tree_to_dot(t, filename):
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
        opts, args = getopt.getopt(argv, "o:")
        for opt, arg in opts:
            if opt == "-o":
                filename=arg
            else:
                usage()
        return (filename, args[0])
    except getopt.GetoptError:
        usage()

def usage():
    print __doc__
    sys.exit()

def main(argv):
    (filename, ts)=get_args(argv)
    (t, ts)=str_to_tree(ts)
    tree_to_dot(t, filename)

if __name__ == "__main__":
    main(sys.argv[1:])

#reference
#[1] http://www.graphviz.org/mywiki/FaqBalanceTree
