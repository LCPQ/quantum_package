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
!provide fock_virt_total_spin_trace
 provide delta_ij_mrpt 
 
 print *,  'N_det    = ', N_det
 print *,  'N_states = ', N_states

 integer :: i
 do i=1,N_states
  print *,  'State    = ', i
  print *,  'PT2      = ', second_order_pt_new(i)
  print *,  'E        = ', CI_energy(i)
  print *,  'E+PT2    = ', CI_energy(i)+second_order_pt_new(i)
  print *,  '-----------------------------'
 enddo
 print *,'****** DIAGONALIZATION OF DRESSED MATRIX ******'
 print *,  'E dressed= ', CI_dressed_pt2_new_energy(i)

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

