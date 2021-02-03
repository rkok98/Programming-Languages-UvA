#!/bin/bash

echo "digraph D {"
for f in $( find .. -name '*.java')
do    
    cat $f | ./remove_comments.sh | perl -0ne 'while (/class\s+(\S+)\s+extends\s+(\S+).*\{.*}/sg) {print "$1 -> $2\n$1 [shape=oval]\n";}'
done
echo "}"
