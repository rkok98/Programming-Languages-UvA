#!/bin/bash

# package\s(\S+)\;

JAVAS=$( find .. -name '*.java')

packages=$(mktemp -d)

for f in $JAVAS
do  
    source=$(cat $f | ./remove_comments.sh)
    
    package=$(echo $source | perl -0ne 'while (/package\s(\S+)\;/sg) {print "$1\n";}')
    class=$(echo $source | perl -0ne 'while (/class\s+(\S+).*?\{.*}/sg) {print "$1\n";}')

    if [ -n "$package" ]; then
        echo $class >> $packages/$package
    fi
done

echo "digraph D {"
for f in $packages/*
do
    package=$(basename $f)

    echo -e "\tsubgraph cluster_X { label = "$package""
    for l in $(cat $f)
    do
        echo -e "\t\t"$l" [shape=box]"
    done
    echo -e "\t}\n"
done
echo "}"