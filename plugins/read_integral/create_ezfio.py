#!/usr/bin/env python
from ezfio import ezfio

import sys
filename = sys.argv[1]
num_elec, nucl_num, mo_tot_num = map(int,sys.argv[2:5])

nuclear_repulsion = float(sys.argv[5])
ezfio.set_file(filename)

#Important !
import math
ezfio.electrons_elec_alpha_num = int(math.ceil(num_elec / 2.))
ezfio.electrons_elec_beta_num = int(math.floor(num_elec / 2.))


#Important
ezfio.set_nuclei_nucl_num(nucl_num)
ezfio.set_nuclei_nucl_charge([0.]*nucl_num)
ezfio.set_nuclei_nucl_coord( [ [0.], [0.], [0.] ]*nucl_num )
ezfio.set_nuclei_nucl_label( ['He'] * nucl_num )

ezfio.set_nuclei_disk_access_nuclear_repulsion('Read')
ezfio.set_nuclei_nuclear_repulsion(nuclear_repulsion)

# Ao num
ao_num = mo_tot_num
ezfio.set_ao_basis_ao_basis("Dummy one. We read MO")
ezfio.set_ao_basis_ao_num(ao_num)
ezfio.set_ao_basis_ao_nucl([1]*ao_num) #Maybe put a realy incorrect stuff

#Just need one
ao_prim_num_max  = 5

d = [ [0] *ao_prim_num_max]*ao_num
ezfio.set_ao_basis_ao_prim_num([ao_prim_num_max]*ao_num)
ezfio.set_ao_basis_ao_power(d)
ezfio.set_ao_basis_ao_coef(d)
ezfio.set_ao_basis_ao_expo(d)

#Dummy one
ao_md5 = '3b8b464dfc95f282129bde3efef3c502'
ezfio.set_ao_basis_ao_md5(ao_md5)
ezfio.set_mo_basis_ao_md5(ao_md5)


ezfio.set_mo_basis_mo_tot_num(mo_tot_num)
ezfio.set_mo_basis_mo_coef([ [0]*mo_tot_num] * ao_num)
