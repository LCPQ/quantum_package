program cisd
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_det=10000
  do i=1,N_det
    do k=1,N_int
      psi_det(k,1,i) = psi_det_sorted(k,1,i)
      psi_det(k,2,i) = psi_det_sorted(k,2,i)
    enddo
    psi_coef(i,:)  = psi_coef_sorted(i,:)
  enddo
  TOUCH psi_det psi_coef psi_det_sorted psi_coef_sorted psi_average_norm_contrib_sorted N_det
  call save_wavefunction
end
