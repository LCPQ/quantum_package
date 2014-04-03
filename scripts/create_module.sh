#!/bin/bash
# 
# usage:
#  create_module.sh MOs AOs Electrons
# Prepares all the files for the creation of a new module.
# The first argument is the name of the module
# All remaining aruments are dependencies.
# Thu Apr  3 01:44:58 CEST 2014

DEBUG=0

# If DEBUG=1, the print debug info.
function debug()
{
  if [[ $DEBUG -eq 1 ]]
  then
    function debug()
    {
      echo "$@"
    }
  else
    function debug()
    {
    }
  fi
  debug $@
}




MODULE=$1
shift
DEPS=$@




# Check if module already exists
if [ -d ${MODULE} ]
then
  echo "Error: Module $MODULE already exists"
  exit 1
fi

debug "Module does not already exist: OK"


# Set up dependencies
ALL_MODULES=$(cat NEEDED_MODULES)



if [[ 1 -eq 0 ]] ; then # TODO

# Create module directory and go into it
if [[ ! mkdir ${QPACKAGE_ROOT}/src/${MODULE} ]]
then
  print "Unable to create module directory."
  exit 1
fi

if [[ ! -d  ${QPACKAGE_ROOT}/src/${MODULE} ]]
then
  print "Something strange happened: the"
  print  ${QPACKAGE_ROOT}/src/${MODULE} 
  print "directory was not created."
  exit 1
fi

cd ${QPACKAGE_ROOT}/src/${MODULE}
if [[ ${PWD} != ${QPACKAGE_ROOT}/src/${MODULE} ]]
then
  print "Something strange happened: we should be in"
  print  ${QPACKAGE_ROOT}/src/${MODULE} 
  print "but we are in"
  print ${PWD}
  exit 1
fi

debug "Module directory is created."


fi  # TODO





# Create the Makefile
${QPACKAGE_ROOT}/create_Makefile.sh


# Update module list in main NEEDED_MODULES
ALL_MODULES+=" ${MODULE}"
cd ${QPACKAGE_ROOT}/src
echo ${ALL_MODULES} > NEEDED_MODULES
debug "Updated NEEDED_MODULES"

