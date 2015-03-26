#!/bin/bash
#
# Useful functions in all scripts

# Make a list unique
function unique_list()
{
  for d in $@
  do
    echo $d
  done | sort | uniq
}

function check_current_dir_is_src()
{
  if [[ "${PWD}" == "${QPACKAGE_ROOT}/src" ]]
  then
     return 0
  else
     echo "Current directory should be \$QPACKAGE_ROOT/src"
     exit -1
  fi
}

function check_current_dir_is_module()
{
  cd ..
  if [[ "${PWD}" == "${QPACKAGE_ROOT}/src" ]]
  then
     cd $OLDPWD
     return 0
  else
     cd $OLDPWD
     echo "Current directory should be \$QPACKAGE_ROOT/src"
     exit -1
  fi
}
if [[ -f NEEDED_MODULES ]]
then
  NEEDED_MODULES=$(cat NEEDED_MODULES)
fi

# List of executables in the current directory
function ls_exe()
{
  find . -perm /u+x -type f
}

function asksure() {
  echo -n $@ "(Y/N) "
  answer=w
  while [[ $answer != [YyNn] ]]
  do
     read answer
     [[ $answer = [Yy] ]] && retval=0 || retval=1
  done
  return $retval
}

function error()
{
  echo "-------------------- Error --------------------"
  printf "$@\n"
  echo "-----------------------------------------------"
  exit -1
}

if [[ -z ${DEBUG} ]]
then
  function info ()
  {
    :
  }
else
  function info ()
  {
    echo "$@"
  }
fi

