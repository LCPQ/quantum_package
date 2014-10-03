program save_fock_orb

  double precision, allocatable :: one_body_dm_ao_alpha(:,:)
  double precision, allocatable :: one_body_dm_ao_beta (:,:)

  read_wf = .True.
  touch read_wf
  print *,  'N_det = ',N_det
  print *, HF_energy
  allocate (one_body_dm_ao_alpha(ao_num_align,ao_num),               &
            one_body_dm_ao_beta (ao_num_align,ao_num))

  call mo_to_ao_no_overlap(one_body_dm_mo_alpha,mo_tot_num_align, &
                one_body_dm_ao_alpha,ao_num_align)
  call mo_to_ao_no_overlap(one_body_dm_mo_beta ,mo_tot_num_align, &
                one_body_dm_ao_beta ,ao_num_align)

  do i=1,mo_tot_num
   do j=1,mo_tot_num
     if (abs(fock_matrix_mo(i,j)) > 1.d-10) then
       print *, i,j, fock_matrix_mo(i,j)
     endif
   enddo
  enddo
  hf_density_matrix_ao_alpha = one_body_dm_ao_alpha
  hf_density_matrix_ao_beta  = one_body_dm_ao_beta
  touch hf_density_matrix_ao_alpha hf_density_matrix_ao_beta

  print *,  '---'
  do i=1,mo_tot_num
   do j=1,mo_tot_num
     if (abs(fock_matrix_mo(i,j)) > 1.d-10) then
       print *, i,j, fock_matrix_mo(i,j)
     endif
   enddo
  enddo
  mo_coef = eigenvectors_fock_matrix_mo
  mo_label = 'CASSCF'
  TOUCH mo_coef mo_label 
  print *, HF_energy
  call save_mos

  deallocate(one_body_dm_ao_alpha,one_body_dm_ao_beta)
end

