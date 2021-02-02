#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# Usage:    ./rename_name.sh <name-old> <name-new>
#
# This script replaces given names with new given names.

readonly START_DIRECTORY=./..
readonly SOURCE_SUFFIX=.java

readonly OLD_CLASS=$1
readonly NEW_CLASS=$2

find $START_DIRECTORY -name $OLD_CLASS$SOURCE_SUFFIX  -exec mv {} $NEW_CLASS$SOURCE_SUFFIX

