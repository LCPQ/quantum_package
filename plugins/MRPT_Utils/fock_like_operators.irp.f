 BEGIN_PROVIDER [double precision, fock_core_inactive, (mo_tot_num)]
 BEGIN_DOC
! inactive part of the fock operator with contributions only from the inactive
 END_DOC
 implicit none
 integer :: i,j
 double precision :: accu

 integer :: j_inact_core_orb,i_inact_core_orb
 do i = 1, n_core_inact_orb
  accu = 0.d0
  i_inact_core_orb = list_core_inact(i)
  do j = 1, n_core_inact_orb
   j_inact_core_orb = list_core_inact(j)
   accu += 2.d0 * mo_bielec_integral_jj(i_inact_core_orb,j_inact_core_orb)  & 
                - mo_bielec_integral_jj_exchange(i_inact_core_orb,j_inact_core_orb)
  enddo
  fock_core_inactive(i_inact_core_orb) = accu + mo_mono_elec_integral(i_inact_core_orb,i_inact_core_orb)
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [double precision, fock_virt_from_core_inact, (mo_tot_num)]
 BEGIN_DOC
! fock operator for the virtuals that comes from the doubly occupied orbitals
 END_DOC
 implicit none
 integer :: i,j
 double precision :: accu

 integer :: j_inact_core_orb,i_virt_orb
 do i = 1, n_virt_orb
  accu = 0.d0
  i_virt_orb = list_virt(i)
  do j = 1, n_core_inact_orb
! do j = 1, elec_alpha_num
!  j_inact_core_orb = j
   j_inact_core_orb = list_core_inact(j)
   accu += 2.d0 * mo_bielec_integral_jj(i_virt_orb,j_inact_core_orb)  & 
                - mo_bielec_integral_jj_exchange(i_virt_orb,j_inact_core_orb)
  enddo
  fock_virt_from_core_inact(i_virt_orb) = accu 
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [double precision, fock_core_inactive_from_act, (mo_tot_num,2,N_states)]
 BEGIN_DOC
! inactive part of the fock operator with contributions only from the active
 END_DOC
 implicit none
 integer :: i,j,k
 double precision :: accu_coulomb,accu_exchange(2)
 double precision :: na,nb,ntot
 double precision :: coulomb, exchange
 double precision :: get_mo_bielec_integral
 integer :: j_act_orb,k_act_orb,i_inact_core_orb
 integer :: i_state

 do i_state = 1,N_states
  do i = 1, n_core_inact_orb
   accu_coulomb = 0.d0
   accu_exchange = 0.d0
   i_inact_core_orb = list_core_inact(i)
   do j = 1, n_act_orb
    j_act_orb = list_act(j)
    na = one_body_dm_mo_alpha(j_act_orb,j_act_orb,i_state)
    nb = one_body_dm_mo_beta(j_act_orb,j_act_orb,i_state)
    ntot = na + nb
    coulomb = mo_bielec_integral_jj(i_inact_core_orb,j_act_orb)
    exchange = mo_bielec_integral_jj_exchange(i_inact_core_orb,j_act_orb)
    accu_coulomb += ntot * coulomb
    accu_exchange(1) += na * exchange
    accu_exchange(2) += nb * exchange
    do k = j+1, n_act_orb
     k_act_orb = list_act(k)
     na = one_body_dm_mo_alpha(j_act_orb,k_act_orb,i_state)
     nb = one_body_dm_mo_beta(j_act_orb,k_act_orb,i_state)
     ntot = na + nb
     coulomb = get_mo_bielec_integral(j_act_orb,i_inact_core_orb,k_act_orb,i_inact_core_orb,mo_integrals_map) 
     exchange = get_mo_bielec_integral(j_act_orb,k_act_orb,i_inact_core_orb,i_inact_core_orb,mo_integrals_map)
     accu_coulomb += 2.d0 * ntot * coulomb
     accu_exchange(1) += 2.d0 * na * exchange
     accu_exchange(2) += 2.d0 * nb * exchange
    enddo
   enddo
   fock_core_inactive_from_act(i_inact_core_orb,1,i_state) = accu_coulomb - accu_exchange(1) 
   fock_core_inactive_from_act(i_inact_core_orb,2,i_state) = accu_coulomb - accu_exchange(2) 
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [double precision, fock_virt_from_act, (mo_tot_num,2,N_states)]
 BEGIN_DOC
! virtual part of the fock operator with contributions only from the active
 END_DOC
 implicit none
 integer :: i,j,k
 double precision :: accu_coulomb,accu_exchange(2)
 double precision :: na,nb,ntot
 double precision :: coulomb, exchange
 double precision :: get_mo_bielec_integral
 integer :: j_act_orb,i_virt_orb,k_act_orb
 integer :: i_state
 ! TODO : inverse loop of i_state 

 do i_state = 1, N_states
  do i = 1, n_virt_orb
   accu_coulomb = 0.d0
   accu_exchange = 0.d0
   i_virt_orb = list_virt(i)
   do j = 1, n_act_orb
    j_act_orb = list_act(j)
    na = one_body_dm_mo_alpha(j_act_orb,j_act_orb,i_state)
    nb = one_body_dm_mo_beta(j_act_orb,j_act_orb,i_state)
    ntot = na + nb
    coulomb = mo_bielec_integral_jj(i_virt_orb,j_act_orb)
    exchange = mo_bielec_integral_jj_exchange(i_virt_orb,j_act_orb)
    accu_coulomb += ntot * coulomb
    accu_exchange(1) += na * exchange
    accu_exchange(2) += nb * exchange
    do k = j+1, n_act_orb
     k_act_orb = list_act(k)
     na = one_body_dm_mo_alpha(j_act_orb,k_act_orb,i_state)
     nb = one_body_dm_mo_beta(j_act_orb,k_act_orb,i_state)
     ntot = na + nb
     coulomb = get_mo_bielec_integral(j_act_orb,i_virt_orb,k_act_orb,i_virt_orb,mo_integrals_map) 
     exchange = get_mo_bielec_integral(j_act_orb,k_act_orb,i_virt_orb,i_virt_orb,mo_integrals_map)
     accu_coulomb += 2.d0 * ntot * coulomb
     accu_exchange(1) += 2.d0 * na * exchange
     accu_exchange(2) += 2.d0 * nb * exchange
    enddo
   enddo
   fock_virt_from_act(i_virt_orb,1,i_state) = accu_coulomb - accu_exchange(1) 
   fock_virt_from_act(i_virt_orb,2,i_state) = accu_coulomb - accu_exchange(2) 
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [double precision, fock_core_inactive_total, (mo_tot_num,2,N_states)]
&BEGIN_PROVIDER [double precision, fock_core_inactive_total_spin_trace, (mo_tot_num,N_states)]
 BEGIN_DOC
! inactive part of the fock operator 
 END_DOC
 implicit none
 integer :: i
 integer :: i_inact_core_orb
 integer :: i_state
 do i_state = 1, N_states
  do i = 1, n_core_inact_orb
   i_inact_core_orb = list_core_inact(i)
   fock_core_inactive_total(i_inact_core_orb,1,i_state) = fock_core_inactive(i_inact_core_orb) + fock_core_inactive_from_act(i_inact_core_orb,1,i_state)
   fock_core_inactive_total(i_inact_core_orb,2,i_state) = fock_core_inactive(i_inact_core_orb) + fock_core_inactive_from_act(i_inact_core_orb,2,i_state)
   fock_core_inactive_total_spin_trace(i_inact_core_orb,i_state) = 0.5d0 * (fock_core_inactive_total(i_inact_core_orb,1,i_state) + fock_core_inactive_total(i_inact_core_orb,2,i_state))
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [double precision, fock_virt_total, (mo_tot_num,2,N_states)]
&BEGIN_PROVIDER [double precision, fock_virt_total_spin_trace, (mo_tot_num,N_states)]
 BEGIN_DOC
! inactive part of the fock operator 
 END_DOC
 implicit none
 integer :: i
 integer :: i_virt_orb
 integer :: i_state
 do i_state = 1, N_states
  do i = 1, n_virt_orb
   i_virt_orb= list_virt(i)
   fock_virt_total(i_virt_orb,1,i_state) = fock_virt_from_core_inact(i_virt_orb) + fock_virt_from_act(i_virt_orb,1,i_state)+ mo_mono_elec_integral(i_virt_orb,i_virt_orb)
   fock_virt_total(i_virt_orb,2,i_state) = fock_virt_from_core_inact(i_virt_orb) + fock_virt_from_act(i_virt_orb,2,i_state)+ mo_mono_elec_integral(i_virt_orb,i_virt_orb)
   fock_virt_total_spin_trace(i_virt_orb,i_state) = 0.5d0 * ( fock_virt_total(i_virt_orb,1,i_state) + fock_virt_total(i_virt_orb,2,i_state) )
  enddo
 enddo
 END_PROVIDER





 BEGIN_PROVIDER [double precision, fock_operator_active_from_core_inact, (mo_tot_num,mo_tot_num)]
 BEGIN_DOC
! active part of the fock operator with contributions only from the inactive
 END_DOC
 implicit none
 integer :: i,j,k,k_inact_core_orb
 integer :: iorb,jorb
 double precision :: accu
 double precision               :: get_mo_bielec_integral,coulomb, exchange
 PROVIDE mo_bielec_integrals_in_map
 fock_operator_active_from_core_inact = 0.d0
 do i = 1, n_act_orb
  iorb = list_act(i)
  do j = 1, n_act_orb
   jorb = list_act(j)
   accu = 0.d0
   do k = 1, n_core_inact_orb 
    k_inact_core_orb = list_core_inact(k)
    coulomb  = get_mo_bielec_integral(k_inact_core_orb,iorb,k_inact_core_orb,jorb,mo_integrals_map)
    exchange = get_mo_bielec_integral(k_inact_core_orb,jorb,iorb,k_inact_core_orb,mo_integrals_map)
    accu += 2.d0 * coulomb - exchange
   enddo
   fock_operator_active_from_core_inact(iorb,jorb) = accu
  enddo
 enddo

 END_PROVIDER 

 


