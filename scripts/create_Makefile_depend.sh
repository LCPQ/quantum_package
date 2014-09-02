#!/bin/bash
# 
# This script is automatically invoked by Makefiles and should not be invoked
# by users.
# Creates the Makefile.depend file. This file contains all the external source
# files included by including other modules.
# Thu Apr  3 01:44:09 CEST 2014

SRC=""
OBJ=""
DEPS="$(cat NEEDED_MODULES)"

for M in ${DEPS}
do
  # X is the list of external source files
  X=$(grep 'SRC=' "${QPACKAGE_ROOT}/src/${M}/Makefile" 2>/dev/null |cut -d '=' -f 2) 
  for f in ${X}
  do
    SRC+=" ${M}/${f}"
  done
  X=$(grep 'OBJ=' "${QPACKAGE_ROOT}/src/${M}/Makefile" 2>/dev/null |cut -d '=' -f 2) 
  for f in ${X}
  do
    OBJ+=" IRPF90_temp/${M}/${f/IRPF90_temp//}"
  done
done

# Create the Makefile.depend
cat << EOF > Makefile.depend
# This file was created by the $0 script. Do not modify it by hand.

SRC+=${SRC}
OBJ+=${OBJ}
EOF


