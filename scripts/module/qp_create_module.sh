#!/bin/bash
# 
# usage:
#  create_module.sh MOs AOs Electrons
# Prepares all the files for the creation of a new module.
# The first argument is the name of the module
# All remaining aruments are dependencies.
# Thu Apr  3 01:44:58 CEST 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

check_current_dir_is_src

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
      :
    }
  fi
  debug $@
}

function fail()
{
  echo "Error:  $@"
  cd "${QPACKAGE_ROOT}/src"
  rm -rf -- "${MODULE}"
  exit 1
}



MODULE=$1

# Check command line
if [[ -z $MODULE ]]
then
  echo "usage: $(basename $0) <NewModuleName>"
  exit 1
fi

shift



# Check if module already exists
if [ -d ${MODULE} ]
then
  echo "Error: Module $MODULE already exists"
  exit 1
fi

debug "Module does not already exist: OK"


# Set up dependencies
ALL_MODULES="$(cat NEEDED_MODULES)"
echo "Select which modules you are sure you will need:  (press q to quit)"  
NEEDED_MODULES=""
select M in ${ALL_MODULES}
do
  if [[ -z $M ]]
  then
    break
  fi
  NEEDED_MODULES+=" $M"
  echo "$NEEDED_MODULES"
done



# Create module directory and go into it
mkdir "${QPACKAGE_ROOT}/src/${MODULE}"
if [[ $? != 0 ]]
then
  echo  "Error: Unable to create module directory."
  exit 1
fi

if [[ ! -d  "${QPACKAGE_ROOT}/src/${MODULE}" ]]
then
  echo  "Something strange happened: the"
  echo  "${QPACKAGE_ROOT}/src/${MODULE}"
  echo  "directory was not created."
  exit 1
fi

cd "${QPACKAGE_ROOT}/src/${MODULE}"
if [[ "${PWD}" != "${QPACKAGE_ROOT}/src/${MODULE}" ]]
then
  echo  "Something strange happened: we should be in"
  echo  "${QPACKAGE_ROOT}/src/${MODULE}"
  echo  "but we are in"
  echo  "${PWD}"
  exit 1
fi

debug "Module directory is created."



# Create the Makefile
"${QPACKAGE_ROOT}/scripts/module/create_Makefile.sh" || fail "Unable to create Makefile"
if [[ ! -f Makefile ]]
then
  fail "Makefile was not created"
fi
debug "Makefile created"

# Create the NEEDED_MODULES file
echo "$NEEDED_MODULES" > NEEDED_CHILDREN_MODULES
debug "NEEDED_MODULES created"

# Create rst templates
"${QPACKAGE_ROOT}/scripts/module/create_rst_templates.sh" || fail "Unable to create rst templates"


# Update module list in main NEEDED_MODULES
ALL_MODULES+=" ${MODULE}"
echo "${ALL_MODULES}" > "${QPACKAGE_ROOT}/src/NEEDED_MODULES"
debug "Updated NEEDED_MODULES"


