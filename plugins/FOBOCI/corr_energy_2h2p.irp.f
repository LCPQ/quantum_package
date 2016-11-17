 BEGIN_PROVIDER [double precision, corr_energy_2h2p_per_orb_ab, (mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h2p_ab_2_orb, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h2p_bb_2_orb, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h2p_for_1h1p_a, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h2p_for_1h1p_b, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h2p_for_1h1p_double, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h2p_per_orb_aa, (mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h2p_per_orb_bb, (mo_tot_num)]
&BEGIN_PROVIDER [ double precision, total_corr_e_2h2p]
 use bitmasks
 print*,''
 print*,'Providing the 2h2p correlation energy'
 print*,''
 implicit none
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: i,j,k,l
 integer :: i_hole,j_hole,k_part,l_part
 double precision :: get_mo_bielec_integral,hij,delta_e,exc,contrib
 double precision :: diag_H_mat_elem
 integer :: i_ok,ispin
 ! Alpha - Beta correlation energy
 total_corr_e_2h2p = 0.d0
 corr_energy_2h2p_ab_2_orb = 0.d0
 corr_energy_2h2p_bb_2_orb = 0.d0
 corr_energy_2h2p_per_orb_ab = 0.d0
 corr_energy_2h2p_per_orb_aa = 0.d0
 corr_energy_2h2p_per_orb_bb = 0.d0
 corr_energy_2h2p_for_1h1p_a = 0.d0
 corr_energy_2h2p_for_1h1p_b = 0.d0
 corr_energy_2h2p_for_1h1p_double = 0.d0
 do i = 1, n_inact_orb    ! beta 
  i_hole = list_inact(i)
  do k = 1, n_virt_orb    ! beta
   k_part = list_virt(k)
   do j = 1, n_inact_orb  ! alpha
    j_hole = list_inact(j)
    do l = 1, n_virt_orb  ! alpha
     l_part = list_virt(l)

     key_tmp = ref_bitmask
     ispin = 2
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 1
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = (ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))

     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     contrib = hij*hij/delta_e
     total_corr_e_2h2p += contrib
     ! Single orbital contribution
     corr_energy_2h2p_per_orb_ab(i_hole) += contrib
     corr_energy_2h2p_per_orb_ab(k_part) += contrib
     ! Couple of orbital contribution for the single 1h1p
     corr_energy_2h2p_for_1h1p_a(j_hole,l_part) += contrib
     corr_energy_2h2p_for_1h1p_a(l_part,j_hole) += contrib
     corr_energy_2h2p_for_1h1p_b(j_hole,l_part) += contrib
     corr_energy_2h2p_for_1h1p_b(l_part,j_hole) += contrib
     ! Couple of orbital contribution for the double 1h1p
     corr_energy_2h2p_for_1h1p_double(i_hole,l_part) += contrib
     corr_energy_2h2p_for_1h1p_double(l_part,i_hole) += contrib

     corr_energy_2h2p_ab_2_orb(i_hole,j_hole) += contrib
     corr_energy_2h2p_ab_2_orb(j_hole,i_hole) += contrib
     corr_energy_2h2p_ab_2_orb(i_hole,k_part) += contrib
     corr_energy_2h2p_ab_2_orb(k_part,i_hole) += contrib
     corr_energy_2h2p_ab_2_orb(k_part,l_part) += contrib
     corr_energy_2h2p_ab_2_orb(l_part,k_part) += contrib
    enddo 
   enddo
  enddo
 enddo

 ! alpha alpha correlation energy
 do i = 1, n_inact_orb
  i_hole = list_inact(i)
  do j = i+1, n_inact_orb
   j_hole = list_inact(j)
   do k = 1, n_virt_orb
    k_part = list_virt(k)
    do l = k+1,n_virt_orb
     l_part = list_virt(l)
     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     exc = get_mo_bielec_integral(i_hole,j_hole,l_part,k_part,mo_integrals_map)
     key_tmp = ref_bitmask
     ispin = 1
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 1
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))
     hij = hij  - exc 
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))
     total_corr_e_2h2p += contrib
     ! Single orbital contribution
     corr_energy_2h2p_per_orb_aa(i_hole) += contrib
     corr_energy_2h2p_per_orb_aa(k_part) += contrib
     ! Couple of orbital contribution for the single 1h1p
     corr_energy_2h2p_for_1h1p_a(i_hole,k_part) += contrib
     corr_energy_2h2p_for_1h1p_a(k_part,i_hole) += contrib
    enddo 
   enddo
  enddo
 enddo

 ! beta beta correlation energy
 do i = 1, n_inact_orb
  i_hole = list_inact(i)
  do j = i+1, n_inact_orb
   j_hole = list_inact(j)
   do k = 1, n_virt_orb
    k_part = list_virt(k)
    do l = k+1,n_virt_orb
     l_part = list_virt(l)
     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     exc = get_mo_bielec_integral(i_hole,j_hole,l_part,k_part,mo_integrals_map)
     key_tmp = ref_bitmask
     ispin = 2
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 2
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))
     hij = hij  - exc 
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))
     total_corr_e_2h2p += contrib
     ! Single orbital contribution
     corr_energy_2h2p_per_orb_bb(i_hole) += contrib
     corr_energy_2h2p_per_orb_bb(k_part) += contrib
     corr_energy_2h2p_for_1h1p_b(i_hole,k_part) += contrib
     corr_energy_2h2p_for_1h1p_b(k_part,i_hole) += contrib

     ! Two particle correlation energy
     corr_energy_2h2p_bb_2_orb(i_hole,j_hole) += contrib
     corr_energy_2h2p_bb_2_orb(j_hole,i_hole) += contrib
     corr_energy_2h2p_bb_2_orb(i_hole,k_part) += contrib
     corr_energy_2h2p_bb_2_orb(k_part,i_hole) += contrib
     corr_energy_2h2p_bb_2_orb(k_part,l_part) += contrib
     corr_energy_2h2p_bb_2_orb(l_part,k_part) += contrib

    enddo 
   enddo
  enddo
 enddo

END_PROVIDER 

 BEGIN_PROVIDER [double precision, corr_energy_2h1p_ab_bb_per_2_orb, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h1p_for_1h1p_a, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h1p_for_1h1p_b, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h1p_for_1h1p_double, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h1p_per_orb_ab, (mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h1p_per_orb_aa, (mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_2h1p_per_orb_bb, (mo_tot_num)]
&BEGIN_PROVIDER [ double precision, total_corr_e_2h1p]
 use bitmasks
 implicit none
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: i,j,k,l
 integer :: i_hole,j_hole,k_part,l_part
 double precision :: get_mo_bielec_integral,hij,delta_e,exc,contrib
 double precision :: diag_H_mat_elem
 integer :: i_ok,ispin
 ! Alpha - Beta correlation energy
 total_corr_e_2h1p = 0.d0
 corr_energy_2h1p_per_orb_ab = 0.d0
 corr_energy_2h1p_per_orb_aa = 0.d0
 corr_energy_2h1p_per_orb_bb = 0.d0
 corr_energy_2h1p_ab_bb_per_2_orb = 0.d0
 corr_energy_2h1p_for_1h1p_a = 0.d0
 corr_energy_2h1p_for_1h1p_b = 0.d0
 corr_energy_2h1p_for_1h1p_double = 0.d0
 do i = 1, n_inact_orb
  i_hole = list_inact(i)
  do k = 1, n_act_orb
   k_part = list_act(k)
   do j = 1, n_inact_orb
    j_hole = list_inact(j)
    do l = 1, n_virt_orb
     l_part = list_virt(l)

     key_tmp = ref_bitmask
     ispin = 2
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 1
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))

     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))
     total_corr_e_2h1p += contrib
     corr_energy_2h1p_ab_bb_per_2_orb(i_hole,j_hole) += contrib
     corr_energy_2h1p_per_orb_ab(i_hole) += contrib
     corr_energy_2h1p_per_orb_ab(l_part) += contrib
    enddo 
   enddo
  enddo
 enddo

 ! Alpha Alpha spin correlation energy
 do i = 1, n_inact_orb
  i_hole = list_inact(i)
  do j = i+1, n_inact_orb
   j_hole = list_inact(j)
   do k = 1, n_act_orb
    k_part = list_act(k)
    do l = 1,n_virt_orb
     l_part = list_virt(l)
     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     exc = get_mo_bielec_integral(i_hole,j_hole,l_part,k_part,mo_integrals_map)
     key_tmp = ref_bitmask
     ispin = 1
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 1
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))
     hij = hij  - exc 
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))

     total_corr_e_2h1p += contrib
     corr_energy_2h1p_per_orb_aa(i_hole) += contrib
     corr_energy_2h1p_per_orb_aa(l_part) += contrib
    enddo 
   enddo
  enddo
 enddo

 ! Beta Beta correlation energy
 do i = 1, n_inact_orb
  i_hole = list_inact(i)
  do j = i+1, n_inact_orb
   j_hole = list_inact(j)
   do k = 1, n_act_orb
    k_part = list_act(k)
    do l = 1,n_virt_orb
     l_part = list_virt(l)
     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     exc = get_mo_bielec_integral(i_hole,j_hole,l_part,k_part,mo_integrals_map)
     key_tmp = ref_bitmask
     ispin = 2
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 2
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))
     hij = hij  - exc 
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))
     corr_energy_2h1p_ab_bb_per_2_orb(i_hole,j_hole) += contrib

     total_corr_e_2h1p += contrib
     corr_energy_2h1p_per_orb_bb(i_hole) += contrib
     corr_energy_2h1p_per_orb_aa(l_part) += contrib
    enddo 
   enddo
  enddo
 enddo

END_PROVIDER 


 BEGIN_PROVIDER [double precision, corr_energy_1h2p_per_orb_ab, (mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_1h2p_two_orb, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_1h2p_per_orb_aa, (mo_tot_num)]
&BEGIN_PROVIDER [double precision, corr_energy_1h2p_per_orb_bb, (mo_tot_num)]
&BEGIN_PROVIDER [ double precision, total_corr_e_1h2p]
 use bitmasks
 implicit none
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: i,j,k,l
 integer :: i_hole,j_hole,k_part,l_part
 double precision :: get_mo_bielec_integral,hij,delta_e,exc,contrib
 double precision :: diag_H_mat_elem
 integer :: i_ok,ispin
 ! Alpha - Beta correlation energy
 total_corr_e_1h2p = 0.d0
 corr_energy_1h2p_per_orb_ab = 0.d0
 corr_energy_1h2p_per_orb_aa = 0.d0
 corr_energy_1h2p_per_orb_bb = 0.d0
 do i = 1, n_virt_orb
  i_hole = list_virt(i)
  do k = 1, n_act_orb
   k_part = list_act(k)
   do j = 1, n_inact_orb
    j_hole = list_inact(j)
    do l = 1, n_virt_orb
     l_part = list_virt(l)

     key_tmp = ref_bitmask
     ispin = 2
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 1
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))

     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))

     total_corr_e_1h2p += contrib
     corr_energy_1h2p_per_orb_ab(i_hole) += contrib
     corr_energy_1h2p_per_orb_ab(j_hole) += contrib
     corr_energy_1h2p_two_orb(k_part,l_part) += contrib
     corr_energy_1h2p_two_orb(l_part,k_part) += contrib
    enddo 
   enddo
  enddo
 enddo

 ! Alpha Alpha correlation energy
 do i = 1, n_virt_orb
  i_hole = list_virt(i)
  do j = 1, n_inact_orb
   j_hole = list_inact(j)
   do k = 1, n_act_orb
    k_part = list_act(k)
    do l = i+1,n_virt_orb
     l_part = list_virt(l)
     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     exc = get_mo_bielec_integral(i_hole,j_hole,l_part,k_part,mo_integrals_map)

     key_tmp = ref_bitmask
     ispin = 1
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 1
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))
     hij = hij  - exc 
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))
     total_corr_e_1h2p += contrib
     corr_energy_1h2p_per_orb_aa(i_hole) += contrib
     corr_energy_1h2p_per_orb_ab(j_hole) += contrib
     corr_energy_1h2p_two_orb(k_part,l_part) += contrib
     corr_energy_1h2p_two_orb(l_part,k_part) += contrib
    enddo 
   enddo
  enddo
 enddo

 ! Beta Beta correlation energy
 do i = 1, n_virt_orb
  i_hole = list_virt(i)
  do j = 1, n_inact_orb
   j_hole = list_inact(j)
   do k = 1, n_act_orb
    k_part = list_act(k)
    do l = i+1,n_virt_orb
     l_part = list_virt(l)
     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     exc = get_mo_bielec_integral(i_hole,j_hole,l_part,k_part,mo_integrals_map)

     key_tmp = ref_bitmask
     ispin = 2
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 2
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))
     hij = hij  - exc 
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))
     total_corr_e_1h2p += contrib
     corr_energy_1h2p_per_orb_bb(i_hole) += contrib
     corr_energy_1h2p_per_orb_ab(j_hole) += contrib
     corr_energy_1h2p_two_orb(k_part,l_part) += contrib
     corr_energy_1h2p_two_orb(l_part,k_part) += contrib
    enddo 
   enddo
  enddo
 enddo

END_PROVIDER 

 BEGIN_PROVIDER [double precision, corr_energy_1h1p_spin_flip_per_orb, (mo_tot_num)]
&BEGIN_PROVIDER [ double precision, total_corr_e_1h1p_spin_flip]
 use bitmasks
 implicit none
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: i,j,k,l
 integer :: i_hole,j_hole,k_part,l_part
 double precision :: get_mo_bielec_integral,hij,delta_e,exc,contrib
 double precision :: diag_H_mat_elem
 integer :: i_ok,ispin
 ! Alpha - Beta correlation energy
 total_corr_e_1h1p_spin_flip = 0.d0
 corr_energy_1h1p_spin_flip_per_orb = 0.d0
 do i = 1, n_inact_orb
  i_hole = list_inact(i)
  do k = 1, n_act_orb
   k_part = list_act(k)
   do j = 1, n_act_orb
    j_hole = list_act(j)
    do l = 1, n_virt_orb
     l_part = list_virt(l)

     key_tmp = ref_bitmask
     ispin = 2
     call do_mono_excitation(key_tmp,i_hole,k_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     ispin = 1
     call do_mono_excitation(key_tmp,j_hole,l_part,ispin,i_ok)
     if(i_ok .ne.1)cycle
     delta_e  = -(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))

     hij = get_mo_bielec_integral(i_hole,j_hole,k_part,l_part,mo_integrals_map)
     contrib = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij*hij))

     total_corr_e_1h1p_spin_flip += contrib
     corr_energy_1h1p_spin_flip_per_orb(i_hole) += contrib
    enddo 
   enddo
  enddo
 enddo

END_PROVIDER 
