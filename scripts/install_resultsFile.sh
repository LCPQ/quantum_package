#!/bin/bash
#
# Installs the resultsFile Python library
# Mon Jan 19 15:08:18 CET 2015

URL="https://github.com/LCPQ/resultsFile/archive/master.tar.gz"

${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${URL} ${QPACKAGE_ROOT}/resultsFile.tar.gz
tar -zxf resultsFile.tar.gz && rm resultsFile.tar.gz ||exit 1
mv resultsFile-master resultsFile


