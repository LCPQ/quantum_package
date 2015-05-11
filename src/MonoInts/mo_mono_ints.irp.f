BEGIN_PROVIDER [ double precision, mo_mono_elec_integral,(mo_tot_num_align,mo_tot_num)]
  implicit none
  integer :: i,j,n,l
  BEGIN_DOC
 ! array of the mono electronic hamiltonian on the MOs basis
 ! : sum of the kinetic and nuclear electronic potential 
  END_DOC
  do j = 1, mo_tot_num
   do i = 1, mo_tot_num
    mo_mono_elec_integral(i,j) = mo_nucl_elec_integral(i,j) + mo_kinetic_integral(i,j) + mo_pseudo_integral(i,j)
   enddo
  enddo
END_PROVIDER
