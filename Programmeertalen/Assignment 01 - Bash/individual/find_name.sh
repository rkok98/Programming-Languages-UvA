#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# This script prints the file name and line number of each occurrence of
# a given string in all the Java source files in the repository.

readonly START_DIRECTORY=./..
readonly SOURCE_SUFFIX=*.java

grep -niro --include=\*$SOURCE_SUFFIX $1 $START_DIRECTORY