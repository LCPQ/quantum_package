program print_energy
 implicit none
 read_wf = .true.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 integer :: i,j
 double precision :: accu,hij

 print*, 'psi_energy          = ',psi_energy + nuclear_repulsion
 accu = 0.d0
! do i = 1,N_det
!  do j = 1,N_det
!   call i_H_j(psi_det(1,1,j),psi_det(1,1,i),N_int,hij)
!   accu += psi_coef(i,1) * psi_coef(j,1) * hij
!  enddo
! enddo
! print*, 'accu                = ',accu + nuclear_repulsion
end
