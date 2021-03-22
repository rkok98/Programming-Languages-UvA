#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# Usage:    ./generate_diagram_package.sh
#
# This script generates a package diagram from all the java source files

JAVAS=$( find .. -name '*.java')
PACKAGES=$(mktemp -d)

# Removing created temp files in unexpected bash exit
trap 'rm -rf -- "$PACKAGES"' EXIT

for f in $JAVAS
do  
    source=$(cat $f | ./remove_comments.sh)
    
    package=$(echo $source | perl -0ne 'while (/package\s(\S+)\;/sg) {print "$1\n";}')
    class=$(echo $source | perl -0ne 'while (/class\s+(\S+).*?\{.*}/sg) {print "$1\n";}')

    # Write class to file with the name of the package if the package variable is not empty
    if [ -n "$package" ]; then
        echo $class >> $PACKAGES/$package
    fi
done

cluster=0

echo "digraph D {"

for f in $PACKAGES/*
do
    package=$(basename $f)

    # Create package for every package file
    echo -e "\tsubgraph cluster_"$cluster" { label = "$package""

    # Fill package component with classes inside the package file
    for l in $(cat $f)
    do
        echo -e "\t\t"$l" [shape=box]"
    done

    echo -e "\t}\n"

    ((++cluster))
done

echo "}"