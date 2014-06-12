program scf_iteration
  use bitmasks
  implicit none
  double precision               :: SCF_energy_before,SCF_energy_after,diag_H_mat_elem,get_mo_bielec_integral
  double precision               :: E0
  integer                        :: i_it, i, j, k
  integer, allocatable           :: iorder(:)
    double precision, allocatable :: DM_occ(:,:), E_new(:), R(:,:)
   
  E0 = HF_energy 
  i_it = 1
  n_it_scf_max = 100
  SCF_energy_before = 0.d0
  SCF_energy_after = E0
  print *,  E0
  mo_label = "Canonical"
  thresh_SCF = 1.d-10
  DM_occ = mo_density_matrix
  allocate (DM_occ(size(mo_density_matrix,1),mo_tot_num), &
            E_new(mo_tot_num), R(mo_tot_num,mo_tot_num), iorder(mo_tot_num))
  do while (i_it < n_it_scf_max .and. dabs(SCF_energy_before - SCF_energy_after) > thresh_SCF)
    if (SCF_energy_after <= SCF_energy_before+1.d-4) then
      mo_coef = eigenvectors_Fock_matrix_mo
      TOUCH mo_coef mo_label
      DM_occ = mo_density_matrix
    else
      DM_occ = mo_density_matrix
      mo_coef = eigenvectors_Fock_matrix_mo
      TOUCH mo_coef mo_label mo_integrals_map
      DM_occ = DM_occ + 0.0d0*mo_density_matrix
      integer :: rank
      call cholesky_mo(ao_num,mo_tot_num,DM_occ,size(DM_occ,1),mo_coef,size(mo_coef,1),-1.d0,rank)
      print *,  rank
      TOUCH mo_coef mo_label
      call orthonormalize_mos
      call find_rotation(eigenvectors_Fock_matrix_mo,mo_tot_num_align,mo_coef,ao_num,R, mo_tot_num)
       do i=1,mo_tot_num
         iorder(i) = i
         E_new(i) = 0.d0
         do k=1,mo_tot_num
           E_new(i) += R(k,i)*R(k,i)*diagonal_fock_matrix_mo(k)
         enddo
       enddo
       call dsort(E_new(1),iorder(1),mo_tot_num)
       eigenvectors_Fock_matrix_mo = mo_coef
       do j=1,mo_tot_num
         do i=1,ao_num
           mo_coef(i,j) = eigenvectors_Fock_matrix_mo(i,iorder(j))
         enddo
       enddo
      TOUCH mo_coef mo_label mo_integrals_map
    endif
    call clear_mo_map
    SCF_energy_before = SCF_energy_after
    SCF_energy_after = HF_energy
    print*,SCF_energy_after, dabs(SCF_energy_before - SCF_energy_after) 
    i_it +=1
    if(i_it > n_it_scf_max)exit
  enddo
  
  if (i_it >= n_it_scf_max) then
    stop 'Failed'
  endif
  if (SCF_energy_after - E0 > thresh_SCF) then
    stop 'Failed'
  endif
  mo_label = "Canonical"
  deallocate (DM_occ)
  TOUCH mo_label mo_coef
  call save_mos
  
end
