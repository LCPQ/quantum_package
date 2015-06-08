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
  if [[ "${PWD}" == "${QP_ROOT}/src" ]]
  then
     return 0
  else
     echo "Current directory should be \$QP_ROOT/src"
     exit -1
  fi
}

function check_current_dir_is_module()
{
  # If the prefix ${QP_ROOT}/src/ can be removed from $PWD, it means that
  # $PWD is somewhere below ${QP_ROOT}/src/ so it is a module.
  # If the prefix ${QP_ROOT}/src/ can not be removed from $PWD, then
  # "${PWD##${QP_ROOT}/src/}" == "$PWD".

  if [[ "${PWD##${QP_ROOT}/src/}" != "$PWD" ]]
  then
     return 0
  else
     echo "You are not in a submodule"
     exit -1
  fi
}

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

