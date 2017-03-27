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
  write(*,'(A12,X,I3,A3,XX,F20.16)')    '  PT2       ', i,' = ', second_order_pt_new(i)
  write(*,'(A12,X,I3,A3,XX,F22.16)')    '  E         ', i,' = ', psi_ref_average_value(i)
  write(*,'(A12,X,I3,A3,XX,F22.16)')    '  E+PT2     ', i,' = ', psi_ref_average_value(i)+second_order_pt_new(i)
  write(*,'(A12,X,I3,A3,XX,F22.16)')    '  E dressed ', i,' = ', CI_dressed_pt2_new_energy(i)
  write(*,'(A12,X,I3,A3,XX,F20.16)')    '  S^2       ', i,' = ', CI_dressed_pt2_new_eigenvectors_s2(i)
  print*,'coef before and after'
  do j = 1, N_det_ref
   print*,psi_ref_coef(j,i),CI_dressed_pt2_new_eigenvectors(j,i)
  enddo
 enddo 
 if(save_heff_eigenvectors)then
  call save_wavefunction_general(N_det_ref,N_states,psi_ref,N_det_ref,CI_dressed_pt2_new_eigenvectors)
 endif
 if(N_states.gt.1)then
  print*, 'Energy differences : E(i) - E(0)'
  do i = 2, N_States
   print*,'State',i
   write(*,'(A12,X,I3,A3,XX,F20.16)')    '  S^2       ', i,' = ', CI_dressed_pt2_new_eigenvectors_s2(i)
   write(*,'(A12,X,I3,A3,XX,F20.16)')    'Variational ', i,' = ', -(psi_ref_average_value(1) - psi_ref_average_value(i))
   write(*,'(A12,X,I3,A3,XX,F20.16)')    'Perturbative', i,' = ', -(psi_ref_average_value(1)+second_order_pt_new(1) - (psi_ref_average_value(i)+second_order_pt_new(i)))
   write(*,'(A12,X,I3,A3,XX,F20.16)')    'Dressed     ', i,' = ', -( CI_dressed_pt2_new_energy(1) - CI_dressed_pt2_new_energy(i) )
  enddo
 endif

end

subroutine routine_2
 implicit none
 provide electronic_psi_ref_average_value
end

