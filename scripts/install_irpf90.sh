#!/bin/bash
#
# Installs irpf90
# Mon Jan 12 16:00:20 CET 2015

BASE="irpf90"
URL="https://github.com/LCPQ/${BASE}/archive/master.tar.gz"

# Check the QPACKAGE_ROOT directory
if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit 1
fi

cd ${QPACKAGE_ROOT}


${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${URL} ${QPACKAGE_ROOT}/${BASE}.tar.gz
tar -zxf ${BASE}.tar.gz && rm ${BASE}.tar.gz ||exit 1
mv ${BASE}-master irpf90
make -C irpf90 | tee install_irpf90.log
rm -rf -- bin/irpf90 bin/irpman
echo '${QPACKAGE_ROOT}/irpf90/bin/irpf90 $@' > bin/irpf90
echo '${QPACKAGE_ROOT}/irpf90/bin/irpman $@' > bin/irpman
chmod +x bin/irpf90 bin/irpman



