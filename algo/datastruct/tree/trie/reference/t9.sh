
#!/bin/bash
# bash script does something outrageous.
# It attempts to reproduce the T9 capability of the Mobile Phones !

# And does it in fewer commands than before!
# Adapted from Rahul's Original T9.sh

umask 077
NUMN=/tmp/numn.$$
trap "exit 1"    HUP INT PIPE QUIT TERM
trap "rm -f $DICTN $NUMN"     EXIT

cat t9.dic | tr a-z 22233344455566677778889999 > $NUMN

if [ $# -eq 0 ] ; then
    echo "Usage : t9.sh "
    exit 1;
else   
    paste -d: $NUMN t9.dic | grep "^$1" | cut -d":" -f2 | sort
fi