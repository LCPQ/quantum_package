#!/bin/bash
# This script is used by module Makefiles, and should not be used by users.
# Checks if all the other module directories are properly linked in the
# current directory. If not, the links are created.
# Wed Apr  2 14:35:42 CEST 2014

for dir in $@
do
  if [[ ! -h $dir ]] ;
  then
    ln -s ../$dir $dir
  fi
done


