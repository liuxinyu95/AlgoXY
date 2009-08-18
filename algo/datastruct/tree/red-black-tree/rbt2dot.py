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
        self.key = "."; #nil
        self.color = "B";
        self.left = self.right = None

def str_to_tree(ts):
    ts=ts.lstip()
    if ts==".":
        return (None, ts[1:])
    x = Node()
    start=False
    while True:
        c=ts[0]
        if c =='(':
            start = True
            (x.left, ts)=str_to_tree(ts[1:])
            ts
            
            
def tree_to_dot():
    pass

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
    print filename
    print ts

if __name__ == "__main__":
    main(sys.argv[1:])
