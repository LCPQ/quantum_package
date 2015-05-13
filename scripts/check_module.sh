#!/bin/bash
#
# Checks to run in every Module directory before building

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

check_current_dir_is_module

# Check if the NEEDED_MODULES file is consistent
INCLUDE_DIRS="${NEEDED_MODULES} include"
NEEDED_MODULES_OK=$(module_handler.py check_dependencies ${NEEDED_MODULES} )
if [[ $? -ne 0 ]] 
then
  error "
Your NEEDED_MODULES file is inconsistent. It should be
${NEEDED_MODULES_OK}
"
else
  info "NEEDED_MODULES files is consistent"
fi

# Check if README.rst exists
if [[ ! -f README.rst ]] 
then
  ${QPACKAGE_ROOT}/scripts/module/create_rst_templates.sh
  error "
README.rst was not present, so I created a
default one for you.
You should document it before you compile, as
well as the ASSUMPTIONS.rst file.
"
else
  info "README.rst is present"
fi


# Check if ASSUMPTIONS.rst exists
if [[ ! -f ASSUMPTIONS.rst ]] 
then
  error "This is a Bug. At that point, the ASSUMPTIONS.rst) file should exist."
else
  info "ASSUMPTIONS.rst is present."
fi


# Create symbolic links of other modules
if [[ $PWD != ${QPACKAGE_ROOT}/src ]]
then
  for dir in ${INCLUDE_DIRS}
  do
    if [[ ! -h $dir ]] ;
    then
      ln -s ../$dir $dir
    fi
  done
fi

# Update Makefile.depend
${QPACKAGE_ROOT}/scripts/module/create_Makefile_depend.sh

# Update EZFIO interface
${QPACKAGE_ROOT}/scripts/ezfio_interface/ei_handler.py

# Create png
${QPACKAGE_ROOT}/scripts/module/module_handler.py create_png