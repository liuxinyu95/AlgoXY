#!/usr/bin/python

import sys, os, string

def walk(args):
    for arg in args:
        if os.path.isfile(arg):
            insert_margin(arg)
        elif os.path.isdir(arg):
            for path, dirs, files in os.walk(arg):
                for f in files:
                    insert_margin(os.path.join(path, f))

def insert_margin(filename):
    _, ext = os.path.splitext(filename)
    if ext == ".dot":
        print "insert zero margin to", filename
        lines = []
        f = open(filename)
        for line in f:
            if 'margin="0"' not in line:
                lines.append(line)
            if line.startswith("digraph"):
                lines.append('\tmargin="0"\n');
        f.close()
        # print "".join(lines)
        f = open(filename, "w")
        f.write("".join(lines))
        f.close()

if __name__ == "__main__":
    walk(sys.argv[1:])
