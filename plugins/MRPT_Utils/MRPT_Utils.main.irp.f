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
 provide delta_ij 
 
 print *,  'N_det    = ', N_det
 print *,  'N_states = ', N_states
 print *,  'PT2      = ', second_order_pt_new(1)
 print *,  'E        = ', CI_energy
 print *,  'E+PT2    = ', CI_energy+second_order_pt_new(1)
 print *,'****** DIAGONALIZATION OF DRESSED MATRIX ******'
 print *,  'E dressed= ', CI_dressed_pt2_new_energy(1)

end

subroutine routine_2
 implicit none
 integer :: i
 do i = 1, n_core_inact_orb
  print*,fock_core_inactive_total(i,1),fock_core_inactive(i)
 enddo
 double precision :: accu
 accu = 0.d0
 do i = 1, n_act_orb
  integer :: j_act_orb
  j_act_orb = list_act(i)
  accu += one_body_dm_mo_alpha(j_act_orb,j_act_orb)
  print*,one_body_dm_mo_alpha(j_act_orb,j_act_orb),one_body_dm_mo_beta(j_act_orb,j_act_orb) 
 enddo
 print*,'accu = ',accu

end

subroutine routine
 implicit none
 integer :: i,j
 integer :: orb, spin_exc
 integer :: hole_particle
 double precision, allocatable :: norm_out(:)
 allocate(norm_out(N_states_diag))

 orb = list_virt(10)
 hole_particle = -1
 spin_exc = 1 

 call apply_exc_to_psi(orb,hole_particle,spin_exc, & 
           norm_out,psi_det,psi_coef, n_det,psi_det_size,psi_det_size,N_states_diag)
 do i = 1, N_det
  if(psi_coef(i,1).ne.0.d0)then
   print*, ''
   call debug_det(psi_det(1,1,i),N_int)
  print*, 'coef = ',psi_coef(i,1)
  endif
 enddo
 print*,'norm_out = ',norm_out
 
 deallocate(norm_out)

end
