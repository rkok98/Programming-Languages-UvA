#!/bin/bash

JAVAS=$( find .. -name '*.java')

for f in JAVAS
do    
    cat $f | ./remove_comments.sh | perl -0ne 'while (/class\s+(\S+)\s+extends\s+(\S+).*\{.*}/sg) {print "$1 -> $2\n$1 [shape=oval]\n";}'
done