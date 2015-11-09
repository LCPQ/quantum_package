#!/bin/bash
#
# Creates the .gitignore file in the Modules to ignore the symlinks
#
# Tue Jan 13 14:18:05 CET 2015
#

if [[ -z ${QP_ROOT} ]]
then
  print "The QP_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QP_ROOT}/scripts/qp_include.sh

function do_gitingore()
{ 
  cat << EOF > .gitignore
#
# Do not modify this file. Add your ignored files to the gitignore
# (without the dot at the beginning) file.
#
build.ninja
irpf90_entities
irpf90.make
IRPF90_man
IRPF90_temp
Makefile.depend
.ninja_deps
.ninja_log
tags
EOF
  
  if [[ -f gitignore ]]
  then
    cat gitignore >> .gitignore
  fi
  
  find . -type l | sed "s@./@@" >> .gitignore
  find . -type f -executable -print | sed "s@./@@" >> .gitignore
}


if [[ -z $1 ]]
then
  check_current_dir_is_module
  do_gitingore
else
  check_current_dir_is_src
  for i in $@
  do
    if [[ -d $i ]]
    then
       cd $i
       do_gitingore
       cd $OLDPWD
    fi
  done
fi

# Sort the .gitignore to reduce conflict in git merges
sort .gitignore |uniq > .gitignore.new
mv .gitignore.new .gitignore
