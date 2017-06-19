
subroutine give_1h2p_new(matrix_1h2p)
  use bitmasks
 implicit none
 double precision , intent(inout) :: matrix_1h2p(N_det,N_det,*)
 integer :: i,v,r,a,b,c
 integer :: iorb, vorb, rorb, aorb, borb,corb
 integer :: ispin,jspin
 integer :: idet,jdet
 integer(bit_kind) :: perturb_dets(N_int,2,n_act_orb,2,2)
 double precision  :: perturb_dets_phase(n_act_orb,2,2)
 double precision  :: perturb_dets_hij(n_act_orb,2,2)
 double precision  :: perturb_dets_hpsi0(n_act_orb,2,2,N_states)
 logical :: already_generated(n_act_orb,2,2)
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer(bit_kind) :: det_tmp_j(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
 double precision :: accu_contrib(N_states)
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 double precision :: delta_e(n_act_orb,2,N_states)
 double precision :: delta_e_inv(n_act_orb,2,N_states)
 double precision :: delta_e_inactive_virt(N_states)
 integer :: istate
 integer :: index_orb_act_mono(N_det,6)
 integer :: kspin
 double precision :: delta_e_ja(N_states)
 double precision :: hja
 double precision :: contrib_hij 
 double precision :: fock_operator_local(n_act_orb,n_act_orb,2)
 double precision :: hij_test
 integer ::i_ok
 integer(bit_kind) :: det_tmp_bis(N_int,2)
 double precision :: hib , hab 
 double precision :: delta_e_ab(N_states)
 double precision :: hib_test,hja_test,hab_test
 integer :: i_hole,i_part
 double precision :: hia,hjb
 integer :: other_spin(2)
 other_spin(1) = 2
 other_spin(2) = 1

 accu_contrib = 0.d0
!matrix_1h2p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_ref(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_ref(inint,2,1))
 enddo
 do i = 1, n_inact_orb  ! First inactive 
  iorb = list_inact(i)
   do v = 1, n_virt_orb  ! First virtual 
    vorb = list_virt(v)
    do r = 1, n_virt_orb    ! Second virtual
     rorb = list_virt(r) 
     ! take all the integral you will need for i,j,r fixed
     do a = 1, n_act_orb
      aorb = list_act(a)
      active_int(a,1) = get_mo_bielec_integral(iorb,aorb,rorb,vorb,mo_integrals_map) ! direct
      active_int(a,2) = get_mo_bielec_integral(iorb,aorb,vorb,rorb,mo_integrals_map) ! exchange
      perturb_dets_phase(a,1,1) = -1000.d0
      perturb_dets_phase(a,1,2) = -1000.d0
      perturb_dets_phase(a,2,2) = -1000.d0
      perturb_dets_phase(a,2,1) = -1000.d0
     enddo
     

      do istate = 1, N_states
       delta_e_inactive_virt(istate) = &
                                    - fock_virt_total_spin_trace(rorb,istate)               & 
                                    - fock_virt_total_spin_trace(vorb,istate)               & 
                                    + fock_core_inactive_total_spin_trace(iorb,istate)        
      enddo
      do idet = 1, N_det
        call get_excitation_degree_vector_mono_or_exchange(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
        do ispin = 1, 2  ! spin of the couple a-a^dagger (iorb,rorb)
         do jspin = 1, 2   ! spin of the couple a-a^dagger (aorb,vorb)
           do a = 1, n_act_orb      ! First active 
            aorb = list_act(a)
            do istate = 1, N_states
             perturb_dets_hpsi0(a,jspin,ispin,istate) = 0.d0
            enddo
            if(ispin == jspin .and. vorb.le.rorb)cycle ! condition not to double count 
            do inint = 1, N_int
             det_tmp(inint,1) = psi_ref(inint,1,idet)
             det_tmp(inint,2) = psi_ref(inint,2,idet)
            enddo
           ! Do the excitation  inactive -- > virtual
           call clear_bit_to_integer(iorb,det_tmp(1,ispin),N_int)  ! hole in "iorb" of spin Ispin
           call set_bit_to_integer(rorb,det_tmp(1,ispin),N_int)    ! particle in "rorb" of spin Ispin

           ! Do the excitation  active -- > virtual 
           call clear_bit_to_integer(aorb,det_tmp(1,jspin),N_int)  ! hole in "aorb" of spin Jspin
           call set_bit_to_integer(vorb,det_tmp(1,jspin),N_int)    ! particle in "vorb" of spin Jspin

           ! Check if the excitation is possible or not on psi_ref(idet)
           accu_elec= 0
           do inint = 1, N_int
            accu_elec+= popcnt(det_tmp(inint,jspin))
           enddo
           if(accu_elec .ne. elec_num_tab_local(jspin))then
            perturb_dets_phase(a,jspin,ispin) = -1000.0d0
            perturb_dets_hij(a,jspin,ispin) = 0.d0
            cycle
           endif
           do inint = 1, N_int
            perturb_dets(inint,1,a,jspin,ispin) = det_tmp(inint,1) 
            perturb_dets(inint,2,a,jspin,ispin) = det_tmp(inint,2) 
           enddo
            
           call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
           perturb_dets_phase(a,jspin,ispin) = phase
           
           do istate = 1, N_states
            delta_e(a,jspin,istate) = one_anhil(a,jspin,istate)   + delta_e_inactive_virt(istate)
            delta_e_inv(a,jspin,istate) = 1.d0 / delta_e(a,jspin,istate)
           enddo
           if(ispin == jspin)then
            perturb_dets_hij(a,jspin,ispin) = phase * (active_int(a,1) - active_int(a,2) )
           else 
            perturb_dets_hij(a,jspin,ispin) = phase * active_int(a,1) 
           endif
         enddo
        enddo
       enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!! determination of the connections between I and the other J determinants mono excited in the CAS
!!!!!!!!!!!!!!!!!!!!!!!!!!!! the determinants I and J must be connected by the following operator 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | a^{\dagger}_b a_{a}  | Idet> 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | K_{ab}               | Idet>
       do jdet = 1, idx(0)
          if(degree(jdet)==1)then
           call get_mono_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
           if (exc(0,1,1) == 1) then
             ! Mono alpha
             i_hole = list_act_reverse(exc(1,1,1))   !!!  a_a
             i_part = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b}
             kspin = 1                               !!!  kspin 
             index_orb_act_mono(idx(jdet),1) = i_hole
             index_orb_act_mono(idx(jdet),2) = i_part 
             index_orb_act_mono(idx(jdet),3) = kspin                          
             call i_H_j_dyall(psi_active(1,1,idet),psi_active(1,1,idx(jdet)),N_int,hij)
             fock_operator_local(i_hole,i_part,kspin) = hij * phase ! phase less fock operator 
             fock_operator_local(i_part,i_hole,kspin) = hij * phase ! phase less fock operator 
           else
             ! Mono beta
             i_hole = list_act_reverse(exc(1,1,2))   !!!  a_a
             i_part = list_act_reverse(exc(1,2,2))   !!!  a^{\dagger}_{b}
             kspin  = 2                              !!! kspin 
             index_orb_act_mono(idx(jdet),1) = i_hole
             index_orb_act_mono(idx(jdet),2) = i_part 
             index_orb_act_mono(idx(jdet),3) = kspin
             call i_H_j_dyall(psi_active(1,1,idet),psi_active(1,1,idx(jdet)),N_int,hij)
             fock_operator_local(i_hole,i_part,kspin) = hij * phase ! phase less fock operator 
             fock_operator_local(i_part,i_hole,kspin) = hij * phase ! phase less fock operator 
           endif
          else if(degree(jdet)==2)then
           call get_double_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
             ! Mono alpha
             index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,1))   !!!  a_a ALPHA 
             index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b} ALPHA 
             index_orb_act_mono(idx(jdet),3) = 1
             ! Mono beta
             index_orb_act_mono(idx(jdet),4) = list_act_reverse(exc(1,1,2))   !!!  a_a BETA 
             index_orb_act_mono(idx(jdet),5) = list_act_reverse(exc(1,2,2))   !!!  a^{\dagger}_{b} BETA 
             index_orb_act_mono(idx(jdet),6) = 2
          endif
       enddo



       do jdet = 1, idx(0)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CASE OF THE MONO EXCITATIONS 
        if(degree(jdet) == 1)then
          ! two determinants | Idet > and | Jdet > which are connected throw a mono excitation operator 
          ! are connected by the presence of the perturbers determinants |det_tmp>
           aorb  = index_orb_act_mono(idx(jdet),1)   !  a_{aorb}
           borb  = index_orb_act_mono(idx(jdet),2)   !  a^{\dagger}_{borb}
           kspin = index_orb_act_mono(idx(jdet),3)   !  spin of the excitation 
           ! the determinants Idet and Jdet interact throw the following operator 
           !      | Jdet > = a^{\dagger}_{borb,kspin} a_{aorb, kspin} | Idet >
  
           accu_contrib = 0.d0
           do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                            !  a^{\dagger}_r a_{i} (ispin)
            
!           if(ispin == kspin .and. vorb.le.rorb)cycle ! condition not to double count 
            logical :: cycle_same_spin_first_order
            cycle_same_spin_first_order = .False.
            if(ispin == kspin .and. vorb.le.rorb)then
             cycle_same_spin_first_order = .True. 
            endif
!           if(ispin .ne. kspin .and. cycle_same_spin_first_order .eqv. .False. )then ! condition not to double count 
            if(cycle_same_spin_first_order .eqv. .False. )then ! condition not to double count 

            ! FIRST ORDER CONTRIBUTION 
 
            ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{aorb,kspin} a_{iorb,ispin} | Idet > 
             if(perturb_dets_phase(aorb,kspin,ispin) .le. -10.d0)cycle
             do inint = 1, N_int
              det_tmp(inint,1)  = perturb_dets(inint,1,aorb,kspin,ispin) 
              det_tmp(inint,2)  = perturb_dets(inint,2,aorb,kspin,ispin) 
             enddo
             call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
             if(kspin == ispin)then
              hia = phase * (active_int(aorb,1) - active_int(aorb,2) )
             else
              hia = phase * active_int(aorb,1)  
             endif
             call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp,exc,phase,N_int)
             if(kspin == ispin)then
              hja = phase * (active_int(borb,1) - active_int(borb,2) )
             else
              hja = phase * active_int(borb,1)  
             endif
 
             contrib_hij = hja * hia
             do istate = 1, N_states
              accu_contrib(istate) += contrib_hij * delta_e_inv(aorb,kspin,istate)
             enddo
            endif
            !!!! SECOND ORDER CONTRIBTIONS 
           ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,jspin} a_{corb,jspin} a_{iorb,ispin} | Idet > 
            do jspin = 1, 2
             logical :: cycle_same_spin_second_order
             cycle_same_spin_second_order = .False.
             if(ispin == jspin .and. vorb.le.rorb)then
              cycle_same_spin_second_order = .True. 
             endif
             if(cycle_same_spin_second_order .eqv. .False.)then
              do corb = 1, n_act_orb
               if(perturb_dets_phase(corb,jspin,ispin) .le. -10.d0)cycle
               do inint = 1, N_int
                det_tmp(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                det_tmp(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                det_tmp_bis(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                det_tmp_bis(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
               enddo
              ! | det_tmp_bis > = a^{\dagger}_{borb,kspin} a_{aorb,kspin} a_{iorb,ispin} | Idet > 
               call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),kspin,i_ok)
               if(i_ok .ne. 1)cycle
               call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
               hia = perturb_dets_hij(corb,jspin,ispin)
               hab = fock_operator_local(aorb,borb,kspin) * phase

               if(dabs(hia).le.1.d-12)cycle
               if(dabs(hab).le.1.d-12)cycle
 
               call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp_bis,exc,phase,N_int)
               if(jspin == ispin)then
                hjb = phase * (active_int(corb,1) - active_int(corb,2) )
               else
                hjb = phase * active_int(corb,1)  
               endif
               if(dabs(hjb).le.1.d-12)cycle
               do istate = 1, N_states
                accu_contrib(istate)+=hia * delta_e_inv(corb,jspin,istate) &  ! | Idet > --> | det_tmp >
                                     ! | det_tmp > --> | det_tmp_bis > 
                                     *hab / (delta_e(corb,jspin,istate) + one_anhil_one_creat(aorb,borb,kspin,kspin,istate)) &
                                     *hjb
               enddo
              enddo
             endif
            enddo

            

           enddo ! ispin 
           do istate = 1, N_states
            matrix_1h2p(idet,idx(jdet),istate) += accu_contrib(istate)
           enddo

        else if (degree(jdet) == 2)then
         ! CASE OF THE DOUBLE EXCITATIONS, ONLY THIRD ORDER EFFECTS 
         accu_contrib = 0.d0
         do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                          !  a^{\dagger}_r a_{i} (ispin)
           ! if it is standard exchange case, the hole ALPHA == the part. BETA 
           if (index_orb_act_mono(idx(jdet),1)  == index_orb_act_mono(idx(jdet),5))then
            aorb = index_orb_act_mono(idx(jdet),1)  !! the HOLE of the ALPHA electron 
            borb = index_orb_act_mono(idx(jdet),4)  !! the HOLE of the BETA  electron 
            ! first case :: | det_tmp > == a_{borb,\beta} | Idet > 
            cycle_same_spin_second_order = .False.
            if(ispin == 2 .and. vorb.le.rorb)then
             cycle_same_spin_second_order = .True. 
            endif
            if(cycle_same_spin_second_order .eqv. .False.)then ! condition not to double count 
             if(perturb_dets_phase(borb,2,ispin) .le. -10.d0)cycle
             do inint = 1, N_int
              det_tmp(inint,1)  = perturb_dets(inint,1,borb,2,ispin) 
              det_tmp(inint,2)  = perturb_dets(inint,2,borb,2,ispin) 
              det_tmp_bis(inint,1)  = perturb_dets(inint,1,borb,2,ispin) 
              det_tmp_bis(inint,2)  = perturb_dets(inint,2,borb,2,ispin) 
             enddo
             hia = perturb_dets_hij(borb,2,ispin)
             if(dabs(hia).le.1.d-12)cycle
             call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),1,i_ok)
             call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
             hab = fock_operator_local(aorb,borb,1) * phase

             if(dabs(hab).le.1.d-12)cycle
             call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp_bis,exc,phase,N_int)
             if(ispin == 2)then
              hjb = phase * (active_int(aorb,1) - active_int(aorb,2) )
             else if (ispin == 1)then
              hjb = phase * active_int(aorb,1)  
             endif
             if(dabs(hjb).le.1.d-12)cycle
             do istate = 1, N_states
              accu_contrib(istate) += hia * delta_e_inv(borb,2,istate) & ! | Idet > --> | det_tmp >
                                      ! | det_tmp > --> | det_tmp_bis > 
                                    * hab / (delta_e(borb,2,istate) + one_anhil_one_creat(aorb,borb,1,1,istate)) & 
                                    * hjb
             enddo
            endif
            ! second case :: | det_tmp > == a_{aorb,\alpha} | Idet > 
            cycle_same_spin_second_order = .False.
            if(ispin == 1 .and. vorb.le.rorb)then
             cycle_same_spin_second_order = .True. 
            endif
            if(cycle_same_spin_second_order .eqv. .False.)then ! condition not to double count 
             if(perturb_dets_phase(aorb,1,ispin) .le. -10.d0)cycle
             do inint = 1, N_int
              det_tmp(inint,1)  = perturb_dets(inint,1,aorb,1,ispin) 
              det_tmp(inint,2)  = perturb_dets(inint,2,aorb,1,ispin) 
              det_tmp_bis(inint,1)  = perturb_dets(inint,1,aorb,1,ispin) 
              det_tmp_bis(inint,2)  = perturb_dets(inint,2,aorb,1,ispin) 
             enddo
             hia = perturb_dets_hij(aorb,1,ispin)
             if(dabs(hia).le.1.d-12)cycle
             call do_mono_excitation(det_tmp_bis,list_act(borb),list_act(aorb),2,i_ok)
             call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
             hab = fock_operator_local(aorb,borb,2) * phase

             if(dabs(hab).le.1.d-12)cycle
             call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp_bis,exc,phase,N_int)
             if(ispin == 1)then
              hjb = phase * (active_int(borb,1) - active_int(borb,2) )
             else if (ispin == 2)then
              hjb = phase * active_int(borb,1)  
             endif
             if(dabs(hjb).le.1.d-12)cycle
             do istate = 1, N_states
              accu_contrib(istate) += hia * delta_e_inv(aorb,1,istate) & ! | Idet > --> | det_tmp >
                                      ! | det_tmp > --> | det_tmp_bis > 
                                    * hab / (delta_e(aorb,1,istate) + one_anhil_one_creat(borb,aorb,2,2,istate)) & 
                                    * hjb
             enddo
            endif

           ! if it is a closed shell double excitation, the hole ALPHA == the hole BETA 
           else if (index_orb_act_mono(idx(jdet),1) == index_orb_act_mono(idx(jdet),4))then
            aorb = index_orb_act_mono(idx(jdet),1)  !! the HOLE of the ALPHA electron 
            borb = index_orb_act_mono(idx(jdet),2)  !! the PART of the ALPHA electron 
            ! first case :: | det_tmp > == a_{aorb,\beta} | Idet > 
            cycle_same_spin_second_order = .False.
            if(ispin == 2 .and. vorb.le.rorb)then
             cycle_same_spin_second_order = .True. 
            endif
            if(cycle_same_spin_second_order .eqv. .False.)then ! condition not to double count 
             if(perturb_dets_phase(aorb,2,ispin) .le. -10.d0)cycle
             do inint = 1, N_int
              det_tmp(inint,1)  = perturb_dets(inint,1,aorb,2,ispin) 
              det_tmp(inint,2)  = perturb_dets(inint,2,aorb,2,ispin) 
              det_tmp_bis(inint,1)  = perturb_dets(inint,1,aorb,2,ispin) 
              det_tmp_bis(inint,2)  = perturb_dets(inint,2,aorb,2,ispin) 
             enddo
             hia = perturb_dets_hij(aorb,2,ispin)
             if(dabs(hia).le.1.d-12)cycle
             call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),1,i_ok)
             call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
             hab = fock_operator_local(aorb,borb,1) * phase

             if(dabs(hab).le.1.d-12)cycle
             call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp_bis,exc,phase,N_int)
             if(ispin == 2)then
              hjb = phase * (active_int(borb,1) - active_int(borb,2) )
             else if (ispin == 1)then
              hjb = phase * active_int(borb,1)  
             endif
             if(dabs(hjb).le.1.d-12)cycle
             do istate = 1, N_states
              accu_contrib(istate) += hia * delta_e_inv(aorb,2,istate) & ! | Idet > --> | det_tmp >
                                      ! | det_tmp > --> | det_tmp_bis > 
                                    * hab / (delta_e(aorb,2,istate) + one_anhil_one_creat(aorb,borb,1,1,istate)) & 
                                    * hjb
             enddo
            endif

            ! second case :: | det_tmp > == a_{aorb,\alpha} | Idet > 
            cycle_same_spin_second_order = .False.
            if(ispin == 1 .and. vorb.le.rorb)then
             cycle_same_spin_second_order = .True. 
            endif
            if(cycle_same_spin_second_order .eqv. .False.)then ! condition not to double count 
             if(perturb_dets_phase(aorb,1,ispin) .le. -10.d0)cycle
             do inint = 1, N_int
              det_tmp(inint,1)  = perturb_dets(inint,1,aorb,1,ispin) 
              det_tmp(inint,2)  = perturb_dets(inint,2,aorb,1,ispin) 
              det_tmp_bis(inint,1)  = perturb_dets(inint,1,aorb,1,ispin) 
              det_tmp_bis(inint,2)  = perturb_dets(inint,2,aorb,1,ispin) 
             enddo
             hia = perturb_dets_hij(aorb,1,ispin)
             if(dabs(hia).le.1.d-12)cycle
             call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),2,i_ok)
             call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
             hab = fock_operator_local(aorb,borb,2) * phase

             if(dabs(hab).le.1.d-12)cycle
             call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp_bis,exc,phase,N_int)
             if(ispin == 1)then
              hjb = phase * (active_int(borb,1) - active_int(borb,2) )
             else if (ispin == 2)then
              hjb = phase * active_int(borb,1)  
             endif
             if(dabs(hjb).le.1.d-12)cycle
             do istate = 1, N_states
              accu_contrib(istate) += hia * delta_e_inv(aorb,1,istate) & ! | Idet > --> | det_tmp >
                                      ! | det_tmp > --> | det_tmp_bis > 
                                    * hab / (delta_e(aorb,1,istate) + one_anhil_one_creat(aorb,borb,2,2,istate)) & 
                                    * hjb
             enddo
            endif


           else 
           ! one should not fall in this case ...
            call debug_det(psi_ref(1,1,i),N_int)
            call debug_det(psi_ref(1,1,idx(jdet)),N_int)
            call get_double_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
            call decode_exc(exc,2,h1,p1,h2,p2,s1,s2)
            integer                        :: h1, p1, h2, p2, s1, s2
            print*, h1, p1, h2, p2, s1, s2
           
            print*, 'pb !!! it is a double but not an exchange case ....'
            stop
           endif
         enddo ! ispin 
         do istate = 1, N_states
          matrix_1h2p(idet,idx(jdet),istate) += accu_contrib(istate)
         enddo
  
        else if (degree(jdet) == 0)then
        ! diagonal part of the dressing : interaction of | Idet > with all the perturbers generated by the excitations 
        ! 
        ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{aorb,kspin} a_{iorb,ispin} | Idet > 
         accu_contrib = 0.d0
         do ispin = 1, 2
          do kspin = 1, 2
           do a = 1, n_act_orb      ! First active 
           if( perturb_dets_phase(a,kspin,ispin) .le. -10.d0)cycle
            if(ispin == kspin .and. vorb.le.rorb)cycle ! condition not to double count 
            contrib_hij = perturb_dets_hij(a,kspin,ispin) * perturb_dets_hij(a,kspin,ispin)
            do istate = 1, N_states
              accu_contrib(istate) += contrib_hij * delta_e_inv(a,kspin,istate)
            enddo
           enddo
          enddo
         enddo
         do istate = 1, N_states
          matrix_1h2p(idet,idet,istate) += accu_contrib(istate)
         enddo
        
        endif 
       enddo !! jdet 

      
      enddo
    enddo
   enddo
 enddo
end

subroutine give_2h1p_new(matrix_2h1p)
  use bitmasks
 implicit none
 double precision , intent(inout) :: matrix_2h1p(N_det,N_det,*)
 integer :: i,j,r,a,b
 integer :: iorb, jorb, rorb, aorb, borb
 integer :: ispin,jspin
 integer :: idet,jdet
 integer(bit_kind) :: perturb_dets(N_int,2,n_act_orb,2,2)
 double precision  :: perturb_dets_phase(n_act_orb,2,2)
 double precision  :: perturb_dets_hij(n_act_orb,2,2)
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
 integer :: i_hole,i_part
 double precision :: delta_e_inv(n_act_orb,2,N_states)
 double precision :: fock_operator_local(n_act_orb,n_act_orb,2)
 double precision :: delta_e_inactive_virt(N_states)
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 double precision :: delta_e(n_act_orb,2,N_states)
 integer :: istate
 integer :: index_orb_act_mono(N_det,3)
 integer :: kspin
 double precision :: hij_test
 double precision :: accu_contrib(N_states)
 double precision :: contrib_hij
 double precision :: hja
 integer :: corb,i_ok
 integer(bit_kind) :: det_tmp_bis(N_int,2)
 double precision :: hia,hjb,hab
!matrix_2h1p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_ref(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_ref(inint,2,1))
 enddo
 do i = 1, n_inact_orb  ! First inactive 
  iorb = list_inact(i)
   do j = 1, n_inact_orb  ! Second inactive 
    jorb = list_inact(j)
    do r = 1, n_virt_orb    ! First virtual
     rorb = list_virt(r) 
     ! take all the integral you will need for i,j,r fixed
     do a = 1, n_act_orb
      aorb = list_act(a)
      active_int(a,1) = get_mo_bielec_integral(iorb,jorb,rorb,aorb,mo_integrals_map) ! direct
      active_int(a,2) = get_mo_bielec_integral(iorb,jorb,aorb,rorb,mo_integrals_map) ! exchange
      perturb_dets_phase(a,1,1) = -1000.d0
      perturb_dets_phase(a,1,2) = -1000.d0
      perturb_dets_phase(a,2,2) = -1000.d0
      perturb_dets_phase(a,2,1) = -1000.d0
     enddo
     
     do istate = 1, N_states
      delta_e_inactive_virt(istate) = &
                                    - fock_virt_total_spin_trace(rorb,istate)               & 
                                    + fock_core_inactive_total_spin_trace(iorb,istate)      & 
                                    + fock_core_inactive_total_spin_trace(jorb,istate)  
     enddo

      do idet = 1, N_det
       call get_excitation_degree_vector_mono(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
        do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
         do jspin = 1, 2   ! spin of the couple z-a^dagger (j,a)
          if(ispin == jspin .and. iorb.le.jorb)cycle ! condition not to double count 
           do a = 1, n_act_orb      ! First active 
            aorb = list_act(a)
            do inint = 1, N_int
             det_tmp(inint,1) = psi_ref(inint,1,idet)
             det_tmp(inint,2) = psi_ref(inint,2,idet)
            enddo
           ! Do the excitation  inactive -- > virtual
           call clear_bit_to_integer(iorb,det_tmp(1,ispin),N_int)  ! hole in "iorb" of spin Ispin
           call set_bit_to_integer(rorb,det_tmp(1,ispin),N_int)    ! particle in "rorb" of spin Ispin

           ! Do the excitation  inactive -- > active 
           call clear_bit_to_integer(jorb,det_tmp(1,jspin),N_int)  ! hole in "jorb" of spin Jspin
           call set_bit_to_integer(aorb,det_tmp(1,jspin),N_int) ! particle in "aorb" of spin Jspin

           ! Check if the excitation is possible or not on psi_ref(idet)
           accu_elec= 0
           do inint = 1, N_int
            accu_elec+= popcnt(det_tmp(inint,jspin))
           enddo
           if(accu_elec .ne. elec_num_tab_local(jspin))then
            perturb_dets_phase(a,jspin,ispin) = -1000.0d0
            perturb_dets_hij(a,jspin,ispin) = 0.d0
            cycle
           endif
           do inint = 1, N_int
            perturb_dets(inint,1,a,jspin,ispin) = det_tmp(inint,1) 
            perturb_dets(inint,2,a,jspin,ispin) = det_tmp(inint,2) 
           enddo
           call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
           perturb_dets_phase(a,jspin,ispin) = phase
           do istate = 1, N_states
            delta_e(a,jspin,istate) = one_creat(a,jspin,istate)   + delta_e_inactive_virt(istate)
            delta_e_inv(a,jspin,istate) = 1.d0 / delta_e(a,jspin,istate)
           enddo
           if(ispin == jspin)then
            perturb_dets_hij(a,jspin,ispin) = phase * (active_int(a,1) - active_int(a,2) )
           else 
            perturb_dets_hij(a,jspin,ispin) = phase * active_int(a,1) 
           endif
!!!!!!!!!!!!!!!!!!!!!1 Computation of the coefficient at first order coming from idet 
!!!!!!!!!!!!!!!!!!!!!  for the excitation (i,j)(ispin,jspin)  ---> (r,a)(ispin,jspin)
         enddo
        enddo
       enddo
       
!!!!!!!!!!!!!!!!!!!!!!!!!!! determination of the connections between I and the other J determinants mono excited in the CAS
!!!!!!!!!!!!!!!!!!!!!!!!!!!! the determinants I and J must be connected by the following operator 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | a_{b} a^{\dagger}_a | Idet> 
       do jdet = 1, idx(0)
         if(degree(jdet)==1)then
           call get_mono_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
            if (exc(0,1,1) == 1) then
              ! Mono alpha
              i_part = list_act_reverse(exc(1,2,1))    !  a^{\dagger}_{aorb}
              i_hole = list_act_reverse(exc(1,1,1))    !  a_{borb}
              kspin = 1
              index_orb_act_mono(idx(jdet),1) = i_part   !!! a^{\dagger}_a 
              index_orb_act_mono(idx(jdet),2) = i_hole   !!! a_{b}
              index_orb_act_mono(idx(jdet),3) = 1
              call i_H_j_dyall(psi_active(1,1,idet),psi_active(1,1,idx(jdet)),N_int,hij)
              fock_operator_local(i_hole,i_part,kspin) = hij * phase ! phase less fock operator 
              fock_operator_local(i_part,i_hole,kspin) = hij * phase ! phase less fock operator 
            else
              ! Mono beta
              i_part = list_act_reverse(exc(1,2,2))
              i_hole = list_act_reverse(exc(1,1,2))
              kspin = 2
              index_orb_act_mono(idx(jdet),1) = i_part   !!!  a^{\dagger}_a
              index_orb_act_mono(idx(jdet),2) = i_hole   !!!  a_{b}
              index_orb_act_mono(idx(jdet),3) = 2
              call i_H_j_dyall(psi_active(1,1,idet),psi_active(1,1,idx(jdet)),N_int,hij)
              fock_operator_local(i_hole,i_part,kspin) = hij * phase ! phase less fock operator 
              fock_operator_local(i_part,i_hole,kspin) = hij * phase ! phase less fock operator 
            endif
         endif
       enddo

       do jdet = 1, idx(0)
        ! two determinants | Idet > and | Jdet > which are connected throw a mono excitation operator 
        ! are connected by the presence of the perturbers determinants |det_tmp>
        if(degree(jdet) == 1)then
         aorb  = index_orb_act_mono(idx(jdet),1)   !  a^{\dagger}_{aorb}
         borb  = index_orb_act_mono(idx(jdet),2)   !  a_{borb}
         kspin = index_orb_act_mono(idx(jdet),3)   !  spin of the excitation 
         ! the determinants Idet and Jdet interact throw the following operator 
         !      | Jdet > = a_{borb,kspin} a^{\dagger}_{aorb, kspin} | Idet >

         accu_contrib = 0.d0
         do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                          !  a^{\dagger}_r a_{i} (ispin)
!         if(ispin == kspin .and. iorb.le.jorb)cycle ! condition not to double count 
          logical :: cycle_same_spin_first_order
          cycle_same_spin_first_order = .False.
          if(ispin == kspin .and. iorb.le.jorb)then
           cycle_same_spin_first_order = .True.
          endif
          if(ispin .ne. kspin .or. cycle_same_spin_first_order .eqv. .False. )then! condition not to double count 

           ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{aorb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Idet > 
           do inint = 1, N_int
            det_tmp(inint,1)  = perturb_dets(inint,1,aorb,kspin,ispin) 
            det_tmp(inint,2)  = perturb_dets(inint,2,aorb,kspin,ispin) 
           enddo
           ! you determine the interaction between the excited determinant and the other parent | Jdet >
           ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{borb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Jdet >
           ! hja = < det_tmp | H | Jdet >
           call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp,exc,phase,N_int)
           if(kspin == ispin)then
            hja = phase * (active_int(borb,1) - active_int(borb,2) )
           else
            hja = phase * active_int(borb,1)  
           endif
!!         if(dabs(hja).le.1.d-10)cycle
  
 
           do istate = 1, N_states
            accu_contrib(istate) += hja *  perturb_dets_hij(aorb,kspin,ispin) * delta_e_inv(aorb,kspin,istate)
           enddo
          endif
          logical :: cycle_same_spin_second_order
     !!!! SECOND ORDER CONTRIBUTIONS 
            !!!! SECOND ORDER CONTRIBTIONS 
           ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{corb,jspin} a_{jorb,jspin} a_{iorb,ispin} | Idet > 
            do jspin = 1, 2
             cycle_same_spin_second_order = .False.
             if(ispin == jspin .and. iorb.le.jorb)then
              cycle_same_spin_second_order = .True.
             endif
             if(ispin .ne. jspin .or. cycle_same_spin_second_order .eqv. .False. )then! condition not to double count 
              do corb = 1, n_act_orb
               if(perturb_dets_phase(corb,jspin,ispin) .le. -10.d0)cycle
               do inint = 1, N_int
                det_tmp(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                det_tmp(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                det_tmp_bis(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                det_tmp_bis(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
               enddo
              ! | det_tmp_bis > = a^{\dagger}_{aorb,kspin} a_{borb,kspin} a_{iorb,kspin} | Idet > 
               call do_mono_excitation(det_tmp_bis,list_act(borb),list_act(aorb),kspin,i_ok)
               if(i_ok .ne. 1)cycle
               hia = perturb_dets_hij(corb,jspin,ispin)
               if(dabs(hia).le.1.d-10)cycle
               call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
               hab = fock_operator_local(borb,aorb,kspin) * phase
               if(dabs(hab).le.1.d-10)cycle
              
               call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp_bis,exc,phase,N_int)
               if(jspin == ispin)then
                hjb = phase * (active_int(corb,1) - active_int(corb,2) )
               else
                hjb = phase * active_int(corb,1)  
               endif
               if(dabs(hjb).le.1.d-10)cycle
               do istate = 1, N_states
                accu_contrib(istate)+=hia * delta_e_inv(corb,jspin,istate) &  ! | Idet > --> | det_tmp >
                                     ! | det_tmp > --> | det_tmp_bis > 
                                     *hab / (delta_e(corb,jspin,istate) + one_anhil_one_creat(borb,aorb,kspin,kspin,istate)) &
                                     *hjb
               enddo
              enddo ! jspin
             endif
            enddo
         enddo ! ispin 
         do istate = 1, N_states
          matrix_2h1p(idx(jdet),idet,istate) += accu_contrib(istate)
         enddo

        else if (degree(jdet) == 0 )then
        ! diagonal part of the dressing : interaction of | Idet > with all the perturbers generated by the excitations 
        ! 
        ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{aorb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Idet > 
         accu_contrib = 0.d0
         do ispin = 1, 2
          do kspin = 1, 2
           if(ispin == kspin .and. iorb.le.jorb)cycle ! condition not to double count 
           do a = 1, n_act_orb      ! First active 
            contrib_hij = perturb_dets_hij(a,kspin,ispin) * perturb_dets_hij(a,kspin,ispin)
            if(dabs(contrib_hij).le.1.d-10)cycle
            do istate = 1, N_states
              accu_contrib(istate) +=  contrib_hij * delta_e_inv(a,kspin,istate)
            enddo
           enddo
          enddo
         enddo
         do istate =1, N_states
          matrix_2h1p(idet,idet,istate) += accu_contrib(istate)
         enddo
        
        endif
        
       enddo
      enddo
    enddo
   enddo
 enddo





end


