program qmcpack
  implicit none
  BEGIN_DOC
! Generates a file for QMCPACK 
  END_DOC

  integer :: i,j
  read_wf = .True.
  TOUCH read_wf
  call save_wavefunction
  do j=1,ao_prim_num_max
    do i=1,ao_num
      ao_coef(i,j) = ao_coef(i,j) * ao_coef_normalization_factor(i)
    enddo
  enddo
  call ezfio_set_ao_basis_ao_coef(ao_coef)
  do j=1,mo_tot_num
    do i=1,ao_num
      mo_coef(i,j) *= 1.d0/ao_coef_normalization_factor(i)
    enddo
  enddo
  call save_mos
  call system('rm '//trim(ezfio_filename)//'/mo_basis/ao_md5')
  call system('$QP_ROOT/src/QMC/qp_convert_qmcpack_to_ezfio.py '//trim(ezfio_filename))

end
