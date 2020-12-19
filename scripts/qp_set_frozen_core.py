#!/usr/bin/env python2

import os
import sys

sys.path = [ os.environ["QP_ROOT"]+"/install/EZFIO/Python" ] + sys.path
from ezfio import ezfio

ezfio.set_filename(sys.argv[1])

nb = 0
for charge in ezfio.nuclei_nucl_charge:
   if charge < 5:
      pass
   elif charge < 13:
      nb += 1
   else:
      nb += 5

mo_tot_num = ezfio.mo_basis_mo_tot_num

if len(sys.argv)>2:
  if sys.argv[2] == '-q':
    print nb
    sys.exit(0)

if nb == 0:
  os.system( """qp_set_mo_class -act "[1-%d]" %s"""%(mo_tot_num, sys.argv[1]) )
else:
  os.system( """qp_set_mo_class -core "[1-%d]" -act "[%d-%d]" %s"""%(nb, nb+1, mo_tot_num, sys.argv[1]) )

