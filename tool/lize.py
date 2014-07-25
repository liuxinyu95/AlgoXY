#! /usr/bin/python

#    <one line to give the program's name and a brief idea of what it does.>
#    Copyright (C) <year>  <name of author>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
Add GUN GPL licnese to a file or all files under a folder.
"""

import sys
import os
import string
from time import strftime, localtime

AUTHOR = "Liu Xinyu (liuxinyu95@gmail.com)"

MAGIC_NAME = "GNU General Public License"

GPL = "This program is free software: you can redistribute it and/or modify\n"\
    "it under the terms of the GNU General Public License as published by\n"\
    "the Free Software Foundation, either version 3 of the License, or\n"\
    "(at your option) any later version.\n\n"\
    "This program is distributed in the hope that it will be useful,\n"\
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"\
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"\
    "GNU General Public License for more details.\n\n"\
    "You should have received a copy of the GNU General Public License\n"\
    "along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"

EXT = {"C/C++":".c.cpp.h.hpp.cxx.hxx.cc",
       "Script":".py.pl.pm.sh",
       "Haskell":".hs",
       "Lisp":".scm"}

HEAD = {"C/C++":"/*",  "Script":"",   "Haskell":"{-", "Lisp":"#|"}
TAIL = {"C/C++":" */", "Script":"",   "Haskell":"-}", "Lisp":"|#"}
LEAD = {"C/C++":" * ",  "Script":"# ", "Haskell":" ",  "Lisp":" "}

dict = {}

#for python 2.5 only
def index(xs, f):
    for i in range(len(xs)):
        if f(xs[i]):
            return i
    return -1

def init_languages():
    if len(dict) ==0:
        for lan, exts in EXT.items():
            for e in exts.split('.'):
                dict["."+e]=lan
    return dict

def GPL_comments(filename, lan):
    year = strftime("%Y", localtime())
    return "\n".join([HEAD[lan], 
                      LEAD[lan]+filename,
                      LEAD[lan]+"Copyright (C) "+year+" "+AUTHOR,
                      LEAD[lan], 
                      LEAD[lan]+("\n"+LEAD[lan]).join(GPL.split("\n")),
                      TAIL[lan]])+"\n"
                                           
def lize(filename):
    (_, name) = os.path.split(filename)
    (_, ext) = os.path.splitext(filename)
    if ext in dict:
        print filename
        f=open(filename)
        lines=f.readlines()
        f.close()
        if ("".join(lines)).find(MAGIC_NAME)<0:
            pos=0
            if dict[ext]=="Script":
                pos = 1 + index(lines, lambda x: x.find("#!")>=0)
            lines.insert(pos, GPL_comments(name, dict[ext]))
        os.rename(filename, filename+"~") #make a backup copy
        f=open(filename, "w")
        f.write("".join(lines))
        f.close()

def main(argv):
    init_languages()
    for i in argv:
        if os.path.isfile(i):
            lize(i)
        if os.path.isdir(i):
            for path, dirs, files in os.walk(i):
                for f in files:
                    lize(os.path.join(path, f))

if __name__ == "__main__":
    main(sys.argv[1:])
