#!/bin/bash

# Checks if all the other module directories are properly linked in the
# current directory. If not, the links are created.

for dir in $@
do
  if [[ ! -h $dir ]] ;
  then
    ln -s ../$dir $dir
  fi
done


