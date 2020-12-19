
 BEGIN_PROVIDER [ double precision, dressing_column_h, (N_det,N_states) ]
&BEGIN_PROVIDER [ double precision, dressing_column_s, (N_det,N_states) ]
 implicit none
 BEGIN_DOC
 ! \Delta_{state-specific}. \Psi
 END_DOC

 integer :: i,ii,k,j, l
 double precision :: f, tmp
 double precision, external :: u_dot_v
 
 dressing_column_h(:,:) = 0.d0
 dressing_column_s(:,:) = 0.d0

 do k=1,N_states
   do j = 1, n_det
     dressing_column_h(j,k) = delta_ij(k,j,1) 
     dressing_column_s(j,k) = delta_ij(k,j,2) 
   enddo
!   tmp = u_dot_v(dressing_column_h(1,k), psi_coef(1,k), N_det) &
!     - dressing_column_h(l,k) * psi_coef(l,k)
!   dressing_column_h(l,k) -= tmp * f
!   tmp = u_dot_v(dressing_column_s(1,k), psi_coef(1,k), N_det) &
!     - dressing_column_s(l,k) * psi_coef(l,k)
!   dressing_column_s(l,k) -= tmp * f
 enddo

END_PROVIDER

