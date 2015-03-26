#!/bin/bash
# Makes several checks before compiling.
# Wed Mar 25 21:22:18 CET 2015

# Required EZFIO version
EZFIO_REQUIRED_VERSION=1.1

# Required IRPF90 version
IRPF90_REQUIRED_VERSION=1.5

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

check_current_dir_is_src


# Check if the user's config exists
if [[ ! -f  ${QPACKAGE_ROOT}/src/Makefile.config ]] 
then
  error "
Makefile.config is not present.
You can create Makefile.config
by modifying Makefile.config.example
"
else
  info "Makefile.config is present"
fi


IRPF90_VERSION=$( ${IRPF90} -v )
python << EOF
import sys
irp_version = float("${IRPF90_VERSION}".rsplit('.',1)[0])
irp_required_version = float($IRPF90_REQUIRED_VERSION)
if irp_version < irp_required_version:
  sys.exit(-1)
EOF

if [[ $? -ne 0 ]]
then
  error "
Current IRPF90 version :
\$ ${IRPF90} -v
${IRPF90_VERSION}.

IRPF90 version >= ${IRPF90_REQUIRED_VERSION} is required.
To upgrade IRPF90, run :                       
  ${QPACKAGE_ROOT}/scripts/upgrade_irpf90.sh   
"
else
  info "irpf90 version is OK"
fi


# Check EZFIO version
EZFIO_VERSION=$(cat ${QPACKAGE_ROOT}/EZFIO/version | cut -d '=' -f 2)
python << EOF
import sys
ezfio_version = float("${EZFIO_VERSION}".rsplit('.',1)[0])
ezfio_required_version = float($EZFIO_REQUIRED_VERSION)
if ezfio_version < ezfio_required_version:
  sys.exit(-1)
EOF

if [[ $? -ne 0 ]]
then
  error "
Current EZFIO version : ${EZFIO_VERSION}
EZFIO version >= ${EZFIO_REQUIRED_VERSION} is required.
To upgrade EZFIO, run :                       
  ${QPACKAGE_ROOT}/scripts/upgrade_ezfio.sh   
"
else
  info "EZFIO version is OK"
fi

