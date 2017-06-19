subroutine contrib_1h2p_dm_based(accu)
 implicit none
 integer :: i_i,i_r,i_v,i_a,i_b
 integer :: i,r,v,a,b
 integer :: ispin,jspin
 integer :: istate
 double precision, intent(out) :: accu(N_states)
 double precision :: active_int(n_act_orb,2)
 double precision :: delta_e(n_act_orb,2,N_states)
 double precision :: get_mo_bielec_integral
 accu = 0.d0
!do i_i = 1, 1
 do i_i = 1, n_inact_orb
  i = list_inact(i_i)
! do i_r = 1, 1
  do i_r = 1, n_virt_orb
   r = list_virt(i_r)
!  do i_v = 1, 1
   do i_v = 1, n_virt_orb
    v = list_virt(i_v)
    do i_a = 1, n_act_orb
     a = list_act(i_a)
     active_int(i_a,1) = get_mo_bielec_integral(i,a,r,v,mo_integrals_map) ! direct
     active_int(i_a,2) = get_mo_bielec_integral(i,a,v,r,mo_integrals_map) ! exchange
     do istate = 1, N_states
      do jspin=1, 2
       delta_e(i_a,jspin,istate) = one_anhil(i_a,jspin,istate)                        &
                                 - fock_virt_total_spin_trace(r,istate)               & 
                                 - fock_virt_total_spin_trace(v,istate)               & 
                                 + fock_core_inactive_total_spin_trace(i,istate)        
       delta_e(i_a,jspin,istate) = 1.d0/delta_e(i_a,jspin,istate)  
      enddo
     enddo
    enddo
    do i_a = 1, n_act_orb
     a = list_act(i_a)
     do i_b = 1, n_act_orb
!    do i_b = i_a, i_a
      b = list_act(i_b)
      do ispin = 1, 2 ! spin of (i --> r)
       do jspin = 1, 2 ! spin of (a --> v)
        if(ispin == jspin .and. r.le.v)cycle ! condition not to double count 
        do istate = 1, N_states
         if(ispin == jspin)then
          accu(istate) += (active_int(i_a,1) - active_int(i_a,2)) * one_body_dm_mo_spin_index(a,b,istate,ispin)   &
                                                          * (active_int(i_b,1) - active_int(i_b,2)) & 
                                                          * delta_e(i_a,jspin,istate)
         else 
          accu(istate) += active_int(i_a,1)  * one_body_dm_mo_spin_index(a,b,istate,ispin) * delta_e(i_a,ispin,istate) & 
                        * active_int(i_b,1) 
         endif
        enddo
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo


end

subroutine contrib_2h1p_dm_based(accu)
 implicit none
 integer :: i_i,i_j,i_v,i_a,i_b
 integer :: i,j,v,a,b
 integer :: ispin,jspin
 integer :: istate
 double precision, intent(out) :: accu(N_states)
 double precision :: active_int(n_act_orb,2)
 double precision :: delta_e(n_act_orb,2,N_states)
 double precision :: get_mo_bielec_integral
 accu = 0.d0
 do i_i = 1, n_inact_orb
  i = list_inact(i_i)
  do i_j = 1, n_inact_orb
   j = list_inact(i_j)
   do i_v = 1, n_virt_orb
    v = list_virt(i_v)
    do i_a = 1, n_act_orb
     a = list_act(i_a)
     active_int(i_a,1) = get_mo_bielec_integral(i,j,v,a,mo_integrals_map) ! direct
     active_int(i_a,2) = get_mo_bielec_integral(i,j,a,v,mo_integrals_map) ! exchange
     do istate = 1, N_states
      do jspin=1, 2
!      delta_e(i_a,jspin,istate) = 
!                                
       delta_e(i_a,jspin,istate) = one_creat(i_a,jspin,istate)  - fock_virt_total_spin_trace(v,istate)  &
                                 + fock_core_inactive_total_spin_trace(i,istate)       &
                                 + fock_core_inactive_total_spin_trace(j,istate)        
       delta_e(i_a,jspin,istate) = 1.d0/delta_e(i_a,jspin,istate)  
      enddo
     enddo
    enddo
    do i_a = 1, n_act_orb
     a = list_act(i_a)
     do i_b = 1, n_act_orb
!    do i_b = i_a, i_a
      b = list_act(i_b)
      do ispin = 1, 2 ! spin of (i --> v)
       do jspin = 1, 2 ! spin of (j --> a)
        if(ispin == jspin .and. i.le.j)cycle ! condition not to double count 
        do istate = 1, N_states
         if(ispin == jspin)then
          accu(istate) += (active_int(i_a,1) - active_int(i_a,2)) * one_body_dm_dagger_mo_spin_index(a,b,istate,ispin)   &
                                                          * (active_int(i_b,1) - active_int(i_b,2)) & 
                                                          * delta_e(i_a,jspin,istate)
         else 
          accu(istate) += active_int(i_a,1)  * one_body_dm_dagger_mo_spin_index(a,b,istate,ispin) * delta_e(i_a,ispin,istate) & 
                        * active_int(i_b,1) 
         endif
        enddo
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo


end


!subroutine contrib_2p_dm_based(accu)
!implicit none
!integer :: i_r,i_v,i_a,i_b,i_c,i_d
!integer :: r,v,a,b,c,d
!integer :: ispin,jspin
!integer :: istate
!double precision, intent(out) :: accu(N_states)
!double precision :: active_int(n_act_orb,n_act_orb,2)
!double precision :: delta_e(n_act_orb,n_act_orb,2,2,N_states)
!double precision :: get_mo_bielec_integral
!accu = 0.d0
!do i_r = 1, n_virt_orb
! r = list_virt(i_r)
! do i_v = 1, n_virt_orb
!   v = list_virt(i_v)
!   do i_a = 1, n_act_orb
!    a = list_act(i_a)
!    do i_b = 1, n_act_orb
!     b = list_act(i_b)
!     active_int(i_a,i_b,1) = get_mo_bielec_integral(a,b,r,v,mo_integrals_map) ! direct
!     active_int(i_a,i_b,2) = get_mo_bielec_integral(a,b,v,r,mo_integrals_map) ! direct
!     do istate = 1, N_states
!      do jspin=1, 2 ! spin of i_a
!       do ispin = 1, 2 ! spin of i_b
!        delta_e(i_a,i_b,jspin,ispin,istate) = two_anhil(i_a,i_b,jspin,ispin,istate)    &
!                                  - fock_virt_total_spin_trace(r,istate)               & 
!                                  - fock_virt_total_spin_trace(v,istate)                 
!        delta_e(i_a,i_b,jspin,ispin,istate) = 1.d0/delta_e(i_a,i_b,jspin,ispin,istate)  
!       enddo
!      enddo
!     enddo
!    enddo
!   enddo
!   ! diagonal terms 
!   do i_a = 1, n_act_orb
!    a = list_act(i_a)
!    do i_b = 1, n_act_orb
!     b = list_act(i_b)
!     do ispin = 1, 2 ! spin of (a --> r)
!      do jspin = 1, 2 ! spin of (b --> v)
!       if(ispin == jspin .and. r.le.v)cycle ! condition not to double count 
!       if(ispin == jspin .and. a.le.b)cycle ! condition not to double count 
!       do istate = 1, N_states
!        if(ispin == jspin)then
!         double precision :: contrib_spin
!         if(ispin == 1)then
!          contrib_spin = two_body_dm_aa_diag_act(i_a,i_b)
!         else
!          contrib_spin = two_body_dm_bb_diag_act(i_a,i_b)
!         endif
!         accu(istate) += (active_int(i_a,i_b,1) - active_int(i_a,i_b,2)) * contrib_spin  &
!                       * (active_int(i_a,i_b,1) - active_int(i_a,i_b,2)) & 
!                       * delta_e(i_a,i_b,ispin,jspin,istate)
!        else 
!         accu(istate) += 0.5d0 * active_int(i_a,i_b,1)  * two_body_dm_ab_diag_act(i_a,i_b) * delta_e(i_a,i_b,ispin,jspin,istate) & 
!                               * active_int(i_a,i_b,1) 
!        endif
!       enddo
!      enddo
!     enddo
!    enddo
!   enddo
!  enddo
! enddo


!end

