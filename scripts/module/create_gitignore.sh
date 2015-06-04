#!/bin/bash
#
# Creates the .gitignore file in the Modules to ignore the symlinks
#
# Tue Jan 13 14:18:05 CET 2015
#

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh


function do_gitingore()
{ 
  cat << EOF > .gitignore
#
# Do not modify this file. Add your ignored files to the gitignore
# (without the dot at the beginning) file.
#
IRPF90_temp
IRPF90_man
irpf90.make
tags
Makefile.depend
irpf90_entities
build.ninja
.ninja_log
.ninja_deps
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

