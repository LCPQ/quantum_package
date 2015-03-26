#!/bin/bash
# Prepares the EZFIO library before running make
# Wed Mar 25 21:25:11 CET 2015

echo "Building EZFIO library"
echo "=-=-=-=-=-=-=-=-=-=-=-"

# Interpret all the EZFIO.cfg files
for dir in ${QPACKAGE_ROOT}/src/*/ 
do 
  cd $dir || exit -1
  ${QPACKAGE_ROOT}/scripts/ezfio_interface.py
done

# For old-style directories. Will be removed some day...
cp ${QPACKAGE_ROOT}/src{/*,}/*.ezfio_config ${QPACKAGE_ROOT}/EZFIO/config 


