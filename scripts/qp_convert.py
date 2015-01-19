#!/usr/bin/env python

import sys,os
try:
  QPACKAGE_ROOT = os.environ["QPACKAGE_ROOT"]
except:
  print "Error: QPACKAGE_ROOT environment variable not found."
  sys.exit(1)

sys.path =  [ QPACKAGE_ROOT+"/EZFIO/Python", QPACKAGE_ROOT+"/resultsFile" ]+sys.path
from ezfio import ezfio
import ezfio as ez
print "EZFIO: ", os.path.dirname(ez.__file__)

try:
  from resultsFile import *
except:
  print "Error: resultsFile Python library not installed"
  sys.exit(1)


def write_ezfioFile(res,filename):
  res.clean_uncontractions()
  ezfio.set_file(filename)

# Electrons
  ezfio.set_electrons_elec_alpha_num(res.num_alpha)
  ezfio.set_electrons_elec_beta_num(res.num_beta)

# Nuclei
  ezfio.set_nuclei_nucl_num(len(res.geometry))
  charge = []
  coord = []
  coord_x = []
  coord_y = []
  coord_z = []
  for a in res.geometry:
    charge.append(a.charge)
    if res.units == 'BOHR':
      coord_x.append(a.coord[0])
      coord_y.append(a.coord[1])
      coord_z.append(a.coord[2])
    else:
      coord_x.append(a.coord[0]/a0)
      coord_y.append(a.coord[1]/a0)
      coord_z.append(a.coord[2]/a0)
  ezfio.set_nuclei_nucl_charge(charge)
  label = map(lambda x: x.name, res.geometry)
  ezfio.set_nuclei_nucl_label(label)
  ezfio.set_nuclei_nucl_coord(coord_x+coord_y+coord_z)

# Basis
  basis = res.uncontracted_basis
  geom  = res.geometry

# MO 
  MoTag = res.determinants_mo_type
  ezfio.set_mo_basis_mo_label('Orthonormalized')
  MO_type = MoTag
  allMOs = res.uncontracted_mo_sets[MO_type]


  closed = [ (allMOs[i].eigenvalue,i) for i  in res.closed_mos ]
  active = [ (allMOs[i].eigenvalue,i) for i  in res.active_mos ]
  virtual =[ (allMOs[i].eigenvalue,i) for i  in res.virtual_mos ]

# closed.sort()
# active.sort()
# virtual.sort()
  closed = map( lambda x: x[1], closed)
  active = map( lambda x: x[1], active)
  virtual = map( lambda x: x[1], virtual)
  MOindices = closed + active + virtual

  MOs = []
  for i in MOindices:
    MOs.append(allMOs[i])

  mo_tot_num = len(MOs)
  while len(MOindices) < mo_tot_num:
    MOindices.append(len(MOindices))

  MOmap = list(MOindices)
  for i in range(len(MOindices)):
    MOmap[i] = MOindices.index(i)

  energies = []
  for i in xrange(mo_tot_num):
    energies.append(MOs[i].eigenvalue)

  if res.occ_num is not None:
    OccNum    = []
    for i in MOindices:
      OccNum.append(res.occ_num[MO_type][i])

    while len(OccNum) < mo_tot_num:
      OccNum.append(0.)

  MoMatrix = []
  sym0 = [ i.sym for i in res.mo_sets[MO_type] ]
  sym  = [ i.sym for i in res.mo_sets[MO_type] ]
  for i in xrange(len(sym)):
    sym[MOmap[i]] = sym0[i]

  MoMatrix = []
  for i in xrange(len(MOs)):
     m = MOs[i]
     for coef in m.vector:
         MoMatrix.append(coef)

  while len(MoMatrix) < len(MOs[0].vector)**2:
    MoMatrix.append(0.)

  ezfio.set_mo_basis_mo_tot_num(mo_tot_num)
  ezfio.set_mo_basis_mo_occ(OccNum)


  res.clean_contractions()
  # AO Basis
  import string
  at = []
  num_prim = []
  magnetic_number = []
  angular_number = []
  power_x = []
  power_y = []
  power_z = []
  coefficient = []
  exponent = []
  res.convert_to_cartesian()
  for b in res.basis:
    c = b.center
    for i,atom in enumerate(res.geometry):
      if atom.coord == c:
         at.append(i+1)
    num_prim.append(len(b.prim))
    s = b.sym
    power_x.append( string.count(s,"x") )
    power_y.append( string.count(s,"y") )
    power_z.append( string.count(s,"z") )
    coefficient.append(  b.coef )
    exponent.append( [ p.expo for p in b.prim ] )
  ezfio.set_ao_basis_ao_num(len(res.basis))
  ezfio.set_ao_basis_ao_nucl(at)
  ezfio.set_ao_basis_ao_prim_num(num_prim)
  ezfio.set_ao_basis_ao_power(power_x+power_y+power_z)
  prim_num_max = ezfio.get_ao_basis_ao_prim_num_max()
  len_res_basis = len(res.basis)
  for i in range(len(res.basis)):
    coefficient[i] += [ 0. for j in range(len(coefficient[i]),prim_num_max) ]
    exponent[i]    += [ 0. for j in range(len(exponent[i]),prim_num_max) ]
  coefficient = reduce(lambda x, y: x+y, coefficient, [])
  exponent = reduce(lambda x, y: x+y, exponent, [])
  coef = []
  expo = []
  for i in range(prim_num_max):
   for j in range(i,len(coefficient),prim_num_max):
    coef.append ( coefficient[j] )
    expo.append ( exponent[j] )
  ezfio.set_ao_basis_ao_coef(coef)
  ezfio.set_ao_basis_ao_expo(expo)
  ezfio.set_ao_basis_ao_basis("Read by resultsFile")

# Apply threshold to determinants
  if len(res.determinants) == 1:
    sorted_determinants = [ (-1.,1.,res.determinants[0]) ]
  else:
    sorted_determinants = []
    for i,j in zip(res.det_coefficients[0],res.determinants):
      sorted_determinants.append((-abs(i),i,j))
  sorted_determinants.sort()
  norm = 0.0
  for length, (a,b,c) in enumerate(sorted_determinants):
    if -a < det_threshold:
      length -=1
      break
    norm += a**2
  norm = sqrt(norm)
  length += 1
  for i in xrange(length):
    a = sorted_determinants[i]
    sorted_determinants[i] = (a[0],a[1]/norm,a[2])
  sorted_determinants = sorted_determinants[:length]

# MOs
  mo_tot_num = len(res.mo_sets[MoTag])
  closed_mos = res.closed_mos
  active_mos = res.active_mos
  virtual_mos = res.virtual_mos
  to_remove = []
  to_add = []
  for i in active_mos:
    found = False
    for (a,b,c) in sorted_determinants:
      if i in c['alpha']+c['beta']:
        found = True
        break
    if not found:
      to_remove.append(i)
      to_add.append(i)
  virtual_mos = to_add + virtual_mos
  for i in active_mos:
    always = True
    for (a,b,c) in sorted_determinants:
      if not (i in c['alpha'] and i in c['beta']):
        always = False
        break
    if always:
      to_remove.append(i)
      closed_mos.append(i)
  for i in to_remove:
    active_mos.remove(i)


  MOindices = closed_mos + active_mos + virtual_mos
  while len(MOindices) < mo_tot_num:
    MOindices.append(len(MOindices))
  MOmap = list(MOindices)
  for i in range(len(MOindices)):
    MOmap[i] = MOindices.index(i)


  ezfio.set_mo_basis_mo_tot_num(mo_tot_num)

  mo = []
  for i in MOindices:
    mo.append(res.mo_sets[MoTag][i])

  if len(mo) < mo_tot_num:
    newmo = orbital()
    newmo.eigenvalue = 0.
    newmo.vector = [0. for i in range(mo_tot_num)]
    newmo.vector[len(mo)] = 1.
    while len(mo) < mo_tot_num:
      mo.append(newmo)
  Energies  = [ m.eigenvalue for m in mo ]

  if res.occ_num is not None:
    OccNum    = []
    for i in MOindices:
      OccNum.append(res.occ_num[MoTag][i])

    while len(OccNum) < mo_tot_num:
      OccNum.append(0.)
    ezfio.set_mo_basis_mo_occ(OccNum)

  cls = [ "v" for i in mo ]
  for i in closed_mos:
    cls[MOmap[i]] = 'c'
  for i in active_mos:
    cls[MOmap[i]] = 'a'

  sym0 = [ i.sym for i in res.mo_sets[MoTag] ]
  sym  = [ i.sym for i in res.mo_sets[MoTag] ]
  for i in xrange(len(sym)):
    sym[MOmap[i]] = sym0[i]

  MoMatrix = []
  for m in mo:
     for coef in m.vector:
         MoMatrix.append(coef)
  while len(MoMatrix) < len(mo[0].vector)**2:
    MoMatrix.append(0.)
  ezfio.set_mo_basis_mo_coef(MoMatrix)
  del MoMatrix



if __name__ == '__main__':
  # Check command line

  det_threshold = 0.

  if len(sys.argv) == 2:
   State=0
  elif len(sys.argv) == 3:
   State=int(sys.argv[2])
  else:
   print "usage: "+sys.argv[0]+" file.out [state]"
   sys.exit(2)

  firstArg = sys.argv[1]

  file = getFile(firstArg)
  print firstArg, 'recognized as', str(file).split('.')[-1].split()[0]

  filename = firstArg+".ezfio"
  write_ezfioFile(file,filename)





