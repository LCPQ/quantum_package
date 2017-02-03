program MRPT_Utils
  implicit none
  read_wf = .True.
  touch read_wf
! call routine
! call routine_2
  call routine_3
end


subroutine routine_3
 implicit none
 integer :: i,j
!provide fock_virt_total_spin_trace
 provide delta_ij 
 
 print *,  'N_det    = ', N_det
 print *,  'N_states = ', N_states
 do i = 1, N_States
  print*,'State',i
  write(*,'(A12,X,I3,A3,XX,F16.10)')    '  PT2       ', i,' = ', second_order_pt_new(i)
  write(*,'(A12,X,I3,A3,XX,F16.09)')    '  E         ', i,' = ', psi_ref_average_value(i)
  write(*,'(A12,X,I3,A3,XX,F16.09)')    '  E+PT2     ', i,' = ', psi_ref_average_value(i)+second_order_pt_new(i)
  write(*,'(A12,X,I3,A3,XX,F16.09)')    '  E dressed ', i,' = ', CI_dressed_pt2_new_energy(i)
  write(*,'(A12,X,I3,A3,XX,F16.09)')    '  S^2       ', i,' = ', CI_dressed_pt2_new_eigenvectors_s2(i)
  print*,'coef before and after'
  do j = 1, N_det_ref
   print*,psi_ref_coef(j,i),CI_dressed_pt2_new_eigenvectors(j,i)
  enddo
 enddo 
 if(save_heff_eigenvectors)then
  call save_wavefunction_general(N_det_ref,N_states_diag_heff,psi_ref,N_det_ref,CI_dressed_pt2_new_eigenvectors)
 endif
! print*, 'neutral = ',psi_ref_coef(1,1),CI_dressed_pt2_new_eigenvectors(1,1)
! print*, 'ionic   = ',psi_ref_coef(3,1),CI_dressed_pt2_new_eigenvectors(3,1)

end

subroutine routine_2
 implicit none
 integer :: i
 do i = 1, n_core_inact_orb
  print*,fock_core_inactive_total(i,1,1),fock_core_inactive(i)
 enddo
 double precision :: accu
 accu = 0.d0
 do i = 1, n_act_orb
  integer :: j_act_orb
  j_act_orb = list_act(i)
  accu += one_body_dm_mo_alpha(j_act_orb,j_act_orb,1)
  print*,one_body_dm_mo_alpha(j_act_orb,j_act_orb,1),one_body_dm_mo_beta(j_act_orb,j_act_orb,1) 
 enddo
 print*,'accu = ',accu

end

