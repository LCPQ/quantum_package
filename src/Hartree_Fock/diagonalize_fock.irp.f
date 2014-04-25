subroutine diagonalize_fock()
  implicit none

  double precision, allocatable :: mo_coef_new(:,:), R(:,:),eigvalues(:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN ::  mo_coef_new, R

  allocate(R(size(Fock_matrix_mo,1),mo_tot_num))
  allocate(mo_coef_new(ao_num_align,mo_tot_num),eigvalues(mo_tot_num))
  mo_coef_new = mo_coef

  call lapack_diag(eigvalues,R,Fock_matrix_mo,size(Fock_matrix_mo,1),mo_tot_num)

  call dgemm('N','N',ao_num,mo_tot_num,mo_tot_num,1.d0,mo_coef_new,size(mo_coef_new,1),R,size(R,1),0.d0,mo_coef,size(mo_coef,1))
  deallocate(mo_coef_new,R,eigvalues)

  mo_label = "Canonical"
  SOFT_TOUCH mo_coef mo_label
  call clear_mo_map
end

