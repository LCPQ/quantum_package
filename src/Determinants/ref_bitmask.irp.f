 BEGIN_PROVIDER [ double precision, ref_bitmask_energy ]
&BEGIN_PROVIDER [ double precision, mono_elec_ref_bitmask_energy ]
&BEGIN_PROVIDER [ double precision, kinetic_ref_bitmask_energy ]
&BEGIN_PROVIDER [ double precision, nucl_elec_ref_bitmask_energy ]
&BEGIN_PROVIDER [ double precision, bi_elec_ref_bitmask_energy ]
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Energy of the reference bitmask used in Slater rules
  END_DOC
  
  integer                        :: occ(N_int*bit_kind_size,2)
  integer                        :: i,j
  
  call bitstring_to_list(ref_bitmask(1,1), occ(1,1), i, N_int)
  call bitstring_to_list(ref_bitmask(1,2), occ(1,2), i, N_int)
  
  
  ref_bitmask_energy = 0.d0
  mono_elec_ref_bitmask_energy = 0.d0
  kinetic_ref_bitmask_energy   = 0.d0
  nucl_elec_ref_bitmask_energy = 0.d0
  bi_elec_ref_bitmask_energy = 0.d0
  
  do i = 1, elec_beta_num
    ref_bitmask_energy += mo_mono_elec_integral(occ(i,1),occ(i,1)) + mo_mono_elec_integral(occ(i,2),occ(i,2))
    kinetic_ref_bitmask_energy += mo_kinetic_integral(occ(i,1),occ(i,1)) + mo_kinetic_integral(occ(i,2),occ(i,2))
    nucl_elec_ref_bitmask_energy += mo_nucl_elec_integral(occ(i,1),occ(i,1)) + mo_nucl_elec_integral(occ(i,2),occ(i,2))
  enddo

  do i = elec_beta_num+1,elec_alpha_num
    ref_bitmask_energy += mo_mono_elec_integral(occ(i,1),occ(i,1))
    kinetic_ref_bitmask_energy += mo_kinetic_integral(occ(i,1),occ(i,1)) 
    nucl_elec_ref_bitmask_energy += mo_nucl_elec_integral(occ(i,1),occ(i,1)) 
  enddo
  
  do j= 1, elec_alpha_num
    do i = j+1, elec_alpha_num
      bi_elec_ref_bitmask_energy += mo_bielec_integral_jj_anti(occ(i,1),occ(j,1))
      ref_bitmask_energy += mo_bielec_integral_jj_anti(occ(i,1),occ(j,1))
    enddo
  enddo
  
  do j= 1, elec_beta_num
    do i = j+1, elec_beta_num
      bi_elec_ref_bitmask_energy += mo_bielec_integral_jj_anti(occ(i,2),occ(j,2))
      ref_bitmask_energy += mo_bielec_integral_jj_anti(occ(i,2),occ(j,2))
    enddo
    do i= 1, elec_alpha_num
      bi_elec_ref_bitmask_energy += mo_bielec_integral_jj(occ(i,1),occ(j,2))
      ref_bitmask_energy += mo_bielec_integral_jj(occ(i,1),occ(j,2))
    enddo
  enddo
  mono_elec_ref_bitmask_energy = kinetic_ref_bitmask_energy +   nucl_elec_ref_bitmask_energy
  
END_PROVIDER

