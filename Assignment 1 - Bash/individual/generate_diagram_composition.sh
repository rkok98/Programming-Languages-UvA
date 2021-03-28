#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# Usage:    ./generate_diagram_composition.sh
#
# This script generates a composition diagram from all the java source files

echo "digraph D {"
for f in $( find .. -name '*.java')
do    
    SOURCE=$(cat $f | ./remove_comments.sh)
    CLASS=$(echo $SOURCE | perl -0ne 'while (/class\s+(\S+).*?\{.*}/sg) {print "$1\n";}')

    IMPORTS=$(echo $SOURCE | perl -0ne 'while (/\s*([A-Z]+[a-zA-Z]*)\s[a-zA-Z]+\;|\s*[A-Z]+[a-zA-Z]*\<([A-Z]+[a-zA-Z]*)\>\s*[a-zA-Z]+\;/sg) {print "$1\n";}')

    for i in $IMPORTS
    do
        echo $CLASS' -> '$i' [arrowhead=diamond]'
    done
done
echo "}"