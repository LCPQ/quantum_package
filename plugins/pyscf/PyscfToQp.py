
import numpy,re,sys

def pyscf2QP(cell,mf, kpts=[], int_threshold = 1E-15):
   # The integral will be not printed in they are bellow that


  PBC=False
  ComputeMode= re.split('[. ]', str(mf))
  print 'ComputeMode=',ComputeMode

  for n in ComputeMode:
     if n in ("UHF","KUHF","UKS"):
           sys.exit('Unrestricted calculation unsupported in Quantum Package')
     if n == "pbc":
           PBC=True

  if PBC and len(kpts) == 0:
	sys.exit("ERROR (read!): You need to specify explicit the list of K-point (including gamma)")

  print 'Performing PBC?:',PBC
  if PBC:
      from pyscf.pbc import ao2mo
      from pyscf.pbc import tools
  else:
      from pyscf import ao2mo

  natom = len(cell.atom_coords())
  print 'n_atom',   natom
  print 'num_elec', cell.nelectron
  print 'nucl_num', len(cell.atom_coords())


  print ''
  mo_coeff = mf.mo_coeff # List of mo_coeff for each k-point
  if not PBC:
      nmo = mo_coeff.shape[1]
  else:
    nmo = mo_coeff[0].shape[1]
  

  # Wrote all the parameter need to creat a dummy EZFIO folder who will containt the integral after.
  # More an implentation detail than a real thing
  with open('param','w') as f:
        f.write(' '.join(map(str,(cell.nelectron, nmo, natom))))
  #                             _                             
  # |\ |      _ |  _   _. ._   |_)  _  ._      |  _ o  _  ._  
  # | \| |_| (_ | (/_ (_| |    | \ (/_ |_) |_| | _> | (_) | | 
  #                                    |                      
  
  print 'mf, cell', mf.energy_nuc(), cell.energy_nuc()
  shift = tools.pbc.madelung(cell, numpy.zeros(3))*cell.nelectron * -.5 if PBC else 0
  e_nuc = cell.energy_nuc() + shift

  print 'nucl_repul', e_nuc
  with open('e_nuc','w') as f:
         f.write(str(e_nuc))
 
 
  from itertools import product
 
  # ___                                              
  #  |  ._ _|_  _   _  ._ _. |  _   |\/|  _  ._   _  
  # _|_ | | |_ (/_ (_| | (_| | _>   |  | (_) | | (_) 
  #                 _|                              
  
  if PBC:
        h_ao = ('kinetic', mf.get_hcore(kpts=kpts) ) # Give only one k point ?
        dummy_ao = ('nuclear', numpy.zeros( (len(kpts),nmo,nmo), dtype=numpy.float ))
  else:
        h_ao = ('kinetic', mf.get_hcore() )
        dummy_ao = ('nuclear', numpy.zeros( (nmo,nmo), dtype=numpy.float ))
    
  def gen_mono_MO(mo_coeff,l_int,shift=0):
         # 2Id transfortion Transformation. For now we handle only one or zero K point.
         print 'l_int.shape=',l_int.shape
 
         l_int_mo = reduce(numpy.dot, (mo_coeff.T, l_int, mo_coeff)) #This formula is only right for one kpt.
 
	 print 'l_int_mo=',l_int_mo

         for i,j in product(range(nmo), repeat=2):
                 int_ = l_int_mo[i,j]
                 yield (i+1+shift,j+1+shift, int_)
 
  # Print 
  for name, ao in (h_ao,dummy_ao):
         with open('%s_mo' % name,'w') as f:
		 print '%s_mo' % name
		 if not PBC:
                 	for mono in gen_mono_MO(mo_coeff,ao):
                        	 f.write('%s %s %s\n'% mono)
 		 else:
			for i,(m,a) in enumerate(zip(mo_coeff,ao)):
			   for mono in gen_mono_MO(m,a,i):
                                 f.write('%s %s %s\n'% mono)

  # ___                              _    
  #  |  ._ _|_  _   _  ._ _. |  _   |_) o 
  # _|_ | | |_ (/_ (_| | (_| | _>   |_) | 
  #                 _|                    
  #

  def ao2mo_amazing(mo_coeff):
	if PBC:
		eri_4d= mf.with_df.ao2mo(mo_coeff,compact=False)
	else:
		eri_4d= ao2mo.kernel(cell,mo_coeff,compact=False)
	
	return eri_4d.reshape((nmo,)*4)


  def write_amazing(eri_4d, shift=0):

    # HANDLE 8 FOLD by Scemama way. Maybe we can use compact=True
    for l in range(nmo):
      for k in range(nmo):
        for j in range(l,nmo):
                for i in range(max(j,k),nmo):
                        v = eri_4d[i,k,j,l]
                        if abs(v) > int_threshold:
                              f.write('%s %s %s %s %s\n' % (i+1+shift,j+1+shift,k+1+shift,l+1+shift,v))


  if PBC: 
     eri_4d= mf.with_df.ao2mo(mo_coeff[0],compact=False)
  else: #Molecular
     eri_4d= ao2mo.kernel(cell,mo_coeff,compact=False)

  eri_4d = eri_4d.reshape((nmo,)*4)

  f = open('bielec_mo','w')
  for i,mc in enumerate(mo_coeff):
	eri = ao2mo_amazing(mc)
	write_amazing(eri, nmo*i)


