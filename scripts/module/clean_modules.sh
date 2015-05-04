#!/bin/bash 
#
# Cleans a module directory 

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

function do_clean()
{
  rm -rf -- \
    IRPF90_temp IRPF90_man Makefile.depend \
    $(module_handler.py print_genealogy) include \
    ezfio_interface.irp.f irpf90.make irpf90_entities tags $(ls_exe) *.mod
}

if [[ -z $1 ]]
then
  check_current_dir_is_module
  do_clean
else
  check_current_dir_is_src
  for i in $@
  do
    if [[ -d $i ]]
    then
       cd $i
       do_clean
       cd $OLDPWD
    fi
  done
fi

