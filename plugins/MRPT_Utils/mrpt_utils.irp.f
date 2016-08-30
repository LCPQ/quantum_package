
 BEGIN_PROVIDER [ double precision, delta_ij, (N_det,N_det,N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new, (N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in N_det basis
 END_DOC
 integer :: i,j,m
 delta_ij = 0.d0
 call H_apply_mrpt(delta_ij,N_det)
 double precision :: accu
 accu = 0.d0
 do i = 1, N_det
  do j = 1, N_det
   accu += delta_ij(i,j,1) * psi_coef(i,1) * psi_coef(j,1)
  enddo
  write(*,'(1000(F16.10,x))')delta_ij(i,:,:)
 enddo
 print*, 'accu = ',accu
 second_order_pt_new(1) = accu
END_PROVIDER

