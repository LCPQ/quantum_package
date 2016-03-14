program test_new_new
 implicit none 
 read_wf = .True.
 touch read_wf
 call test
end


subroutine test
 implicit none
 integer :: i,j,k,l
 call diagonalize_CI 
 call set_generators_to_psi_det
 print*,'Initial coefficients'
 do i = 1, N_det
  print*,''
  call debug_det(psi_det(1,1,i),N_int)
  print*,'psi_coef = ',psi_coef(i,1) 
  print*,''
 enddo
 double precision, allocatable :: dressing_matrix(:,:)
 double precision :: hij 
 double precision :: phase
 integer :: n_h,n_p,number_of_holes,number_of_particles
 integer :: exc(0:2,2,2)
 integer :: degree
 integer :: h1,h2,p1,p2,s1,s2
 allocate(dressing_matrix(N_det_generators,N_det_generators))
 do i = 1, N_det_generators
  do j = 1, N_det_generators 
   call i_h_j(psi_det_generators(1,1,i),psi_det_generators(1,1,j),N_int,hij)
   dressing_matrix(i,j) = hij
  enddo
 enddo
 href =  dressing_matrix(1,1)
 print*,'Diagonal part of the dressing'
 do i = 1, N_det_generators
  print*,'delta e = ',dressing_matrix(i,i) - href
 enddo
 call all_single_split(psi_det_generators,psi_coef_generators,N_det_generators,dressing_matrix)
 double precision :: href
 print*,''
 ! One considers that the following excitation classes are not repeatable on the 1h and 1p determinants : 
 !   + 1h1p spin flip
 !   + 2h1p 
 !   + 1h2p 
 ! But the 2h2p are correctly taken into account 
!dressing_matrix(1,1) += total_corr_e_1h2p + total_corr_e_2h1p + total_corr_e_1h1p_spin_flip
!do i = 1, N_det_generators
! dressing_matrix(i,i) += total_corr_e_2h2p 
!  n_h = number_of_holes(psi_det(1,1,i))
!  n_p = number_of_particles(psi_det(1,1,i))
!  if(n_h == 1 .and. n_p ==0)then
!   
!   call get_excitation(ref_bitmask,psi_det_generators(1,1,i),exc,degree,phase,N_int)
!   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
!   print*,''
!   print*,' 1h det '
!   print*,''
!   call debug_det(psi_det_generators(1,1,i),N_int)
!   print*,'h1,p1 = ',h1,p1
!   print*,'total_corr_e_2h2p         ',total_corr_e_2h2p
!   print*,'corr_energy_2h2p_per_orb_ab(h1)',corr_energy_2h2p_per_orb_ab(h1) 
!   print*,'corr_energy_2h2p_per_orb_bb(h1)',corr_energy_2h2p_per_orb_bb(h1) 
!   dressing_matrix(i,i) += -corr_energy_2h2p_per_orb_ab(h1) - corr_energy_2h2p_per_orb_bb(h1)
!   dressing_matrix(1,1) += -corr_energy_2h1p_per_orb_aa(h1) - corr_energy_2h1p_per_orb_ab(h1) -corr_energy_2h1p_per_orb_bb(h1) & 
!                           -corr_energy_1h1p_spin_flip_per_orb(h1)
!  endif
!  if(n_h == 0 .and. n_p ==1)then
!   call get_excitation(ref_bitmask,psi_det_generators(1,1,i),exc,degree,phase,N_int)
!   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
!   print*,''
!   print*,' 1p det '
!   print*,''
!   call debug_det(psi_det_generators(1,1,i),N_int)
!   print*,'h1,p1 = ',h1,p1
!   print*,'total_corr_e_2h2p         ',total_corr_e_2h2p
!   print*,'corr_energy_2h2p_per_orb_ab(p1)',corr_energy_2h2p_per_orb_ab(p1) 
!   print*,'corr_energy_2h2p_per_orb_aa(p1)',corr_energy_2h2p_per_orb_aa(p1) 
!   dressing_matrix(i,i) += -corr_energy_2h2p_per_orb_ab(p1) - corr_energy_2h2p_per_orb_aa(p1)
!   dressing_matrix(1,1) += -corr_energy_1h2p_per_orb_aa(p1) - corr_energy_1h2p_per_orb_ab(p1) -corr_energy_1h2p_per_orb_bb(p1)
!  endif
!enddo
!href =  dressing_matrix(1,1)
!print*,'Diagonal part of the dressing'
!do i = 1, N_det_generators
! print*,'delta e = ',dressing_matrix(i,i) - href
!enddo
 call diag_dressed_matrix_and_set_to_psi_det(psi_det_generators,N_det_generators,dressing_matrix)
 print*,'After dressing matrix'
 print*,''
 print*,''
 do i = 1, N_det
  print*,'psi_coef = ',psi_coef(i,1) 
 enddo
!print*,''
!print*,''
!print*,'Canceling the dressing part of the interaction between 1h and 1p'
!do i = 2, N_det_generators
! do j = i+1, N_det_generators
!  call i_h_j(psi_det_generators(1,1,i),psi_det_generators(1,1,j),N_int,hij)
!  dressing_matrix(i,j) = hij
!  dressing_matrix(j,i) = hij 
! enddo
!enddo
!call diag_dressed_matrix_and_set_to_psi_det(psi_det_generators,N_det_generators,dressing_matrix)
!print*,''
!print*,''
!do i = 1, N_det
! print*,'psi_coef = ',psi_coef(i,1) 
!enddo
!print*,''
!print*,''
!print*,'Canceling the interaction between 1h and 1p'

!print*,''
!print*,''
!do i = 2, N_det_generators
! do j = i+1, N_det_generators
!  dressing_matrix(i,j) = 0.d0
!  dressing_matrix(j,i) = 0.d0
! enddo
!enddo
!call diag_dressed_matrix_and_set_to_psi_det(psi_det_generators,N_det_generators,dressing_matrix)
!do i = 1, N_det
! print*,'psi_coef = ',psi_coef(i,1) 
!enddo
 call save_natural_mos
 deallocate(dressing_matrix)


end
