#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# This script prints the file name and line number of each occurrence of
# a given string in all the Java source files in the repository.

readonly START_DIRECTORY=./..
readonly SOURCE_SUFFIX=*.java

find $START_DIRECTORY -name \*$SOURCE_SUFFIX -type f -exec \
    sed -i '' -e 's/'$1'/'$2'/g' {} + 