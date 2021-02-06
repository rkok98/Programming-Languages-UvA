#!/bin/bash

JAVAS=$( find .. -name '*.java')

echo "digraph D {"
for f in $JAVAS
do    
    SOURCE=$(cat $f | ./remove_comments.sh)
    CLASS=$(echo $SOURCE | perl -0ne 'while (/class\s+(\S+).*?\{.*}/sg) {print "$1\n";}')

    IMPORTS=$(echo $SOURCE | perl -0ne 'while (/import\s\S+\.(\S+)\;/sg) {print "$1\n";}')

    for i in $IMPORTS
    do
        echo $CLASS' -> '$i' [arrowhead=dot]'
    done
done
echo "}"