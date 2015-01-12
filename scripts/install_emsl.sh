#!/bin/bash
#
# Installs EMSL_Basis_Set_Exchange_Local
# Mon Jan 12 12:57:19 CET 2015

BASE="EMSL_Basis_Set_Exchange_Local"
URL="https://github.com/LCPQ/EMSL_Basis_Set_Exchange_Local/archive/master.tar.gz"

${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${URL} ${QPACKAGE_ROOT}/EMSL_Basis.tar.gz
tar -zxf EMSL_Basis.tar.gz && rm EMSL_Basis.tar.gz ||exit 1
mv EMSL_Basis_Set_Exchange_Local-master EMSL_Basis


