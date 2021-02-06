#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# Usage:    ./generate_diagram_all.sh
#
# This script generates a diagram from all the generate_diagram_*.sh scripts

DIAGRAM_PREFIX_SCRIPT="generate_diagram_*.sh"

echo "digraph D {"

for f in $( find . -name "$DIAGRAM_PREFIX_SCRIPT")
do
    if [ "$f" != "$0" ];
    then
        bash $f | perl -0ne 'while (/digraph\sD\s\{(.*)\}/sg) {print "$1\n";}'
    fi
done

echo "}"
