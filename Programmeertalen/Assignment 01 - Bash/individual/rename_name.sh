#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# Usage:    ./rename_name.sh <name-old> <name-new>
#
# This script replaces given names with new given names.

readonly START_DIRECTORY=./..
readonly SOURCE_SUFFIX=*.java

readonly OLD_NAME=$1
readonly NEW_NAME=$2

# The 'find' program recursively searches the start directory for java files, 
# in which the 'sed' application replaces the old names with the new names.
find $START_DIRECTORY -name \*$SOURCE_SUFFIX -type f -exec \
    sed -i '' -e 's/'$OLD_NAME'/'$NEW_NAME'/g' {} + 