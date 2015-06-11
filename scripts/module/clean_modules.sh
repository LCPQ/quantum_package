#!/bin/bash 
#
# Cleans a module directory 

if [[ -z ${QP_ROOT} ]]
then
  print "The QP_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QP_ROOT}/scripts/qp_include.sh

function do_clean()
{
  rm -rf -- \
    IRPF90_temp IRPF90_man \
    $(module_handler.py print_descendant) include \
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

