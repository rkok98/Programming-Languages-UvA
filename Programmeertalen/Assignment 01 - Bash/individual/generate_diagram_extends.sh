#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# Usage:    ./generate_diagram_extends.sh
#
# This script generates a extends diagram from all the java source files

echo "digraph D {"
for f in $( find .. -name '*.java')
do    
    cat $f | ./remove_comments.sh | perl -0ne 'while (/class\s+(\S+)\s+extends\s+(\S+).*\{.*}/sg) {print "$1 -> $2\n$1 [shape=oval]\n";}'
done
echo "}"
