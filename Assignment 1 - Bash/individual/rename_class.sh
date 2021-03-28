#!/bin/bash
#
# Author:   René Kok (13671146)
# Study:    Doorstroomminor Software Engineering UvA
# 
# Usage:    ./rename_class.sh <class-old> <class-new>
#
# This script replaces given class name with new given class name.

readonly START_DIRECTORY=./..
readonly SOURCE_SUFFIX=.java

readonly OLD_CLASS=$1
readonly NEW_CLASS=$2

find $START_DIRECTORY -name $OLD_CLASS$SOURCE_SUFFIX -type f \
  -exec sed -i '' -e 's/'$OLD_CLASS'/'$NEW_CLASS'/g' {} \; \
  -exec mv {} $NEW_CLASS$SOURCE_SUFFIX \; 
