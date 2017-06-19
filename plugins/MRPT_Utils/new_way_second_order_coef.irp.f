subroutine give_2h1p_contrib_sec_order(matrix_2h1p)
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
 double precision  :: coef_perturb_from_idet(n_act_orb,2,2,N_states,3)
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer(bit_kind) :: det_tmp_j(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
 integer :: index_orb_act_mono(N_det,6)
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
     
  integer           :: degree(N_det)
  integer           :: idx(0:N_det)
  double precision :: delta_e(n_act_orb,2,N_states)
  integer :: istate

      do idet = 1, N_det
        call get_excitation_degree_vector_mono_or_exchange(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
!       if(idet == 81)then
!       call get_excitation_degree_vector_mono_or_exchange_verbose(psi_ref(1,1,1),psi_ref(1,1,idet),degree,N_int,N_det,idx)
!       endif
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
            perturb_dets_phase(a,jspin,ispin) = -1000.d0
            perturb_dets_hij(a,jspin,ispin) = 0.d0
            do istate = 1, N_states
             coef_perturb_from_idet(a,jspin,ispin,istate,1) = 0.d0
             coef_perturb_from_idet(a,jspin,ispin,istate,2) = 0.d0
            enddo
            cycle
           endif
           do inint = 1, N_int
            perturb_dets(inint,1,a,jspin,ispin) = det_tmp(inint,1) 
            perturb_dets(inint,2,a,jspin,ispin) = det_tmp(inint,2) 
           enddo
           call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
           perturb_dets_phase(a,jspin,ispin) = phase
           do istate = 1, N_states
            delta_e(a,jspin,istate) = one_creat(a,jspin,istate)                                                          &
                                    - fock_virt_total_spin_trace(rorb,istate)               & 
                                    + fock_core_inactive_total_spin_trace(iorb,istate)      & 
                                    + fock_core_inactive_total_spin_trace(jorb,istate)  
           enddo
           if(ispin == jspin)then
            perturb_dets_hij(a,jspin,ispin) = phase * (active_int(a,2) - active_int(a,1) )
           else 
            perturb_dets_hij(a,jspin,ispin) = phase * active_int(a,1) 
           endif
!!!!!!!!!!!!!!!!!!!!!1 Computation of the coefficient at first order coming from idet 
!!!!!!!!!!!!!!!!!!!!!  for the excitation (i,j)(ispin,jspin)  ---> (r,a)(ispin,jspin)
           do istate = 1, N_states
            coef_perturb_from_idet(a,jspin,ispin,istate,1) = perturb_dets_hij(a,jspin,ispin) / delta_e(a,jspin,istate)
           enddo

         enddo
        enddo
       enddo

 
!!!!!!!!!!!!!!!!!!!!!!!!!!! determination of the connections between I and the other J determinants mono excited in the CAS
!!!!!!!!!!!!!!!!!!!!!!!!!!!! the determinants I and J must be connected by the following operator 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | a^{\dagger}_b a_{a}  | Idet> 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | K_{ab}               | Idet>
 integer :: i_hole,i_part
 double precision :: hij_test
 double precision :: fock_operator_local(n_act_orb,n_act_orb,2)
       do jdet = 1, idx(0)
         if(idx(jdet).ne.idet)then
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
             index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,1))   !!!  a_a
             index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b}
             index_orb_act_mono(idx(jdet),3) = 1
             ! Mono beta
             index_orb_act_mono(idx(jdet),4) = list_act_reverse(exc(1,1,2))   !!!  a_a
             index_orb_act_mono(idx(jdet),5) = list_act_reverse(exc(1,2,2))   !!!  a^{\dagger}_{b}
             index_orb_act_mono(idx(jdet),6) = 2
          endif
         else
          index_orb_act_mono(idx(jdet),1) = -1
         endif
       enddo

       integer :: kspin
       integer :: corb,i_ok
       integer(bit_kind) :: det_tmp_bis(N_int,2)
       double precision :: hib , hab , hja
       double precision :: delta_e_ab(N_states)
       double precision :: hib_test,hja_test,hab_test
       do jdet = 1, idx(0)
        if(idx(jdet).ne.idet)then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CASE OF THE MONO EXCITATIONS 
           if(degree(jdet) == 1)then
       !   ! two determinants | Idet > and | Jdet > which are connected throw a mono excitation operator 
       !   ! are connected by the presence of the perturbers determinants |det_tmp>
            aorb  = index_orb_act_mono(idx(jdet),1)   !  a^{\dagger}_{aorb}
            borb  = index_orb_act_mono(idx(jdet),2)   !  a_{borb}
            kspin = index_orb_act_mono(idx(jdet),3)   !  spin of the excitation 
           do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                            !  a^{\dagger}_r a_{i} (ispin)
       !    ! the determinants Idet and Jdet interact throw the following operator 
       !    !      | Jdet > = a_{borb,kspin} a^{\dagger}_{aorb, kspin} | Idet >
             do jspin = 1, 2 
               if (jspin .ne. kspin)then

                do corb = 1, n_act_orb
                 if(perturb_dets_phase(corb,jspin,ispin).le.-100d0)cycle
       !         ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{corb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Idet > 
                 do inint = 1, N_int
                  det_tmp(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                  det_tmp_bis(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp_bis(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                 enddo
       !         ! < idet | H | det_tmp > = phase * (ir|cv)
                 call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
                 if(ispin == jspin)then
                  hib= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else
                  hib= phase * active_int(corb,1) 
                 endif
                 
                 ! | det_tmp_bis > = a^{\dagger}_{borb,kspin} a_{aorb,kspin} | det_tmp > 
                 call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),kspin,i_ok)
                 if(i_ok .ne. 1)cycle
                 call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)

                 ! < det_tmp     | H | det_tmp_bis >  = F_{aorb,borb}
                 hab = (fock_operator_local(aorb,borb,kspin) ) * phase  
            if(hab /= hab)then ! check NaN
             print*, '1'
             stop
            endif
                 ! < jdet | H | det_tmp_bis > = phase * (ir|cv)
                 call get_double_excitation(det_tmp_bis,psi_ref(1,1,idx(jdet)),exc,phase,N_int)
                 if(ispin == jspin)then
                  hja= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else 
                  hja= phase * (active_int(corb,1))
                 endif
                 do istate = 1, N_states
                  delta_e_ab(istate) = delta_e(corb,jspin,istate) + one_anhil_one_creat(borb,aorb,kspin,kspin,istate)
                  matrix_2h1p(idx(jdet),idet,istate) = matrix_2h1p(idx(jdet),idet,istate) + & 
                                       hib / delta_e(corb,jspin,istate) * hab / delta_e_ab(istate) * hja  
       !                          !   < det_tmp     | H | Idet >         / delta_E (Idet --> det_tmp )
       !                          !   < det_tmp     | H | det_tmp_bis >  / delta_E (Idet --> det_tmp --> det_tmp_bis)
       !                          !   < det_tmp_bis | H | Jdet > 
                 enddo
                enddo ! corb
              else 
                if(ispin == kspin .and. iorb.le.jorb)cycle ! condition not to double count 
                do corb = 1, n_act_orb
                 if(corb == aorb .or. corb == borb) cycle
                 if(perturb_dets_phase(corb,jspin,ispin).le.-100d0)cycle
       !         ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{corb,jspin} a_{iorb,ispin} | Idet > 
                 do inint = 1, N_int
                  det_tmp(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                  det_tmp_bis(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp_bis(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                 enddo
                 ! < idet | H | det_tmp > = phase * ( (ir|cv) - (iv|cr) )
                 call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
                 if(ispin == jspin)then
                  hib= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else
                  hib= phase * active_int(corb,1) 
                 endif
                 ! | det_tmp_bis > = a^{\dagger}_{borb,kspin} a_{aorb,kspin} | det_tmp > 
                 call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),kspin,i_ok)
                 if(i_ok .ne. 1)cycle
                 call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
!                ! < det_tmp | H | det_tmp_bis >  = F_{aorb,borb}
                 hab = fock_operator_local(aorb,borb,kspin) * phase  
            if(hab /= hab)then ! check NaN
             print*, '2'
             stop
            endif
                 ! < jdet | H | det_tmp_bis > = phase * ( (ir|cv) - (iv|cr) )
                 call get_double_excitation(det_tmp_bis,psi_ref(1,1,idx(jdet)),exc,phase,N_int)
                 if(ispin == jspin)then
                  hja= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else 
                  hja= phase * (active_int(corb,1))
                 endif
                 do istate = 1, N_states
                  delta_e_ab(istate) = delta_e(corb,jspin,istate) + one_anhil_one_creat(borb,aorb,kspin,kspin,istate)
                  matrix_2h1p(idx(jdet),idet,istate) = matrix_2h1p(idx(jdet),idet,istate) + & 
                                       hib / delta_e(corb,jspin,istate) * hab / delta_e_ab(istate) * hja  
       !                          !   < det_tmp     | H | Idet >         / delta_E (Idet --> det_tmp )
       !                          !   < det_tmp     | H | det_tmp_bis >  / delta_E (Idet --> det_tmp --> det_tmp_bis)
       !                          !   < det_tmp_bis | H | Jdet > 
                 enddo
                enddo ! corb
              endif
             enddo
            enddo
       !   
          else  !! Double excitation operators
       !  
           if (index_orb_act_mono(idx(jdet),1) == index_orb_act_mono(idx(jdet),5))then  !! spin exchange 
            do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                              !  a^{\dagger}_r a_{i} (ispin)
         !!!  ! first combination of spin :: | det_tmp > = a^{\dagger}_{aorb,beta} | Idet >
              jspin = 2
              aorb = index_orb_act_mono(idx(jdet),1) ! hole of the alpha electron 
              borb = index_orb_act_mono(idx(jdet),2) ! particle of the alpha electron
              if(perturb_dets_phase(aorb,jspin,ispin).le.-100d0)cycle
              do inint = 1, N_int
               det_tmp(inint,1)  = perturb_dets(inint,1,aorb,jspin,ispin) 
               det_tmp(inint,2)  = perturb_dets(inint,2,aorb,jspin,ispin) 
               det_tmp_bis(inint,1)  = perturb_dets(inint,1,aorb,jspin,ispin) 
               det_tmp_bis(inint,2)  = perturb_dets(inint,2,aorb,jspin,ispin) 
              enddo
              ! | det_tmp > = a^{\dagger}_{aorb,beta} | Idet > 
              call get_double_excitation(det_tmp,psi_ref(1,1,idet),exc,phase,N_int)
              if(ispin == jspin)then
               hib= phase * (active_int(aorb,1) - active_int(aorb,2)) 
              else 
               hib= phase * (active_int(aorb,1))
              endif
              if(hib .ne. perturb_dets_hij(aorb,jspin,ispin))then
               print*, 'pb !!'
               print*, 'hib .ne. perturb_dets_hij(aorb,jspin,ispin)'
               stop
              endif
            enddo  !! ispin

           else if(index_orb_act_mono(idx(jdet),1) == index_orb_act_mono(idx(jdet),4))then !! closed shell double excitation
             
           else 
            call get_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,degree_scalar,phase,N_int) 
            integer :: h1,h2,p1,p2,s1,s2 , degree_scalar
            call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
            print*, h1,p1,h2,p2,s1,s2
            call debug_det(psi_ref(1,1,idet),N_int)
            call debug_det(psi_ref(1,1,idx(jdet)),N_int)
            print*, idet,idx(jdet)
            print*, 'pb !!!!!!!!!!!!!'
            call get_excitation_degree_vector_mono_or_exchange_verbose(psi_ref(1,1,1),psi_ref(1,1,idet),degree,N_int,N_det,idx)
            stop
           endif
          endif

        else
       !! diagonal part of the dressing : interaction of | Idet > with all the perturbers generated by the excitations 
       !! 
       !! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{aorb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Idet > 
       !!do ispin = 1, 2
       !! do kspin = 1, 2
       !!  if(ispin == kspin .and. iorb.le.jorb)cycle ! condition not to double count 
       !!  do a = 1, n_act_orb      ! First active 
       !!   do istate = 1, N_states
       !!    matrix_2h1p(idet,idet,istate) += coef_perturb_from_idet(a,kspin,ispin,istate,2) * perturb_dets_hij(a,kspin,ispin) 
       !!   enddo
       !!  enddo
       !! enddo
       !!enddo
       !
        endif
        
       enddo
      enddo
    enddo
   enddo
 enddo





end


subroutine give_1h2p_contrib_sec_order(matrix_1h2p)
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
 double precision  :: coef_perturb_from_idet(n_act_orb,2,2,N_states,2)
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
 double precision :: accu_contrib
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 double precision :: delta_e(n_act_orb,2,N_states)
 integer :: istate
 integer :: index_orb_act_mono(N_det,6)
 double precision :: delta_e_inactive_virt(N_states)
 integer :: kspin
 double precision :: delta_e_ja(N_states)
 double precision :: hja
 double precision :: contrib_hij 
 double precision :: fock_operator_local(n_act_orb,n_act_orb,2)
 double precision :: fock_operator_from_core(n_act_orb,n_act_orb)
 double precision :: fock_operator_from_virt(n_act_orb,n_act_orb)
 double precision :: fock_operator_from_act(n_act_orb,n_act_orb,n_act_orb,2)
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
      already_generated(a,1,1) = .False.
      already_generated(a,1,2) = .False.
      already_generated(a,2,2) = .False.
      already_generated(a,2,1) = .False.
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
             coef_perturb_from_idet(a,jspin,ispin,istate,1) = 0.d0
             coef_perturb_from_idet(a,jspin,ispin,istate,2) = 0.d0
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
            do istate = 1, N_states
             coef_perturb_from_idet(a,jspin,ispin,istate,1) = 0.d0
             coef_perturb_from_idet(a,jspin,ispin,istate,2) = 0.d0
            enddo
            cycle
           endif
           do inint = 1, N_int
            perturb_dets(inint,1,a,jspin,ispin) = det_tmp(inint,1) 
            perturb_dets(inint,2,a,jspin,ispin) = det_tmp(inint,2) 
           enddo
           do inint = 1, N_int
            det_tmp(inint,1) = perturb_dets(inint,1,a,jspin,ispin) 
            det_tmp(inint,2) = perturb_dets(inint,2,a,jspin,ispin) 
           enddo
            
           call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
           perturb_dets_phase(a,jspin,ispin) = phase
           
           do istate = 1, N_states
            delta_e(a,jspin,istate) = one_anhil(a,jspin,istate)   + delta_e_inactive_virt(istate)
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
             integer :: i_hole,i_part
             double precision :: hij_test
       do jdet = 1, idx(0)
         if(idx(jdet).ne.idet)then
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
             index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,1))   !!!  a_a
             index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b}
             index_orb_act_mono(idx(jdet),3) = 1
             ! Mono beta
             index_orb_act_mono(idx(jdet),4) = list_act_reverse(exc(1,1,2))   !!!  a_a
             index_orb_act_mono(idx(jdet),5) = list_act_reverse(exc(1,2,2))   !!!  a^{\dagger}_{b}
             index_orb_act_mono(idx(jdet),6) = 2
          endif
         else
          index_orb_act_mono(idx(jdet),1) = -1
         endif
       enddo

       integer ::dorb,i_ok
       integer(bit_kind) :: det_tmp_bis(N_int,2)
       double precision :: hib , hab 
       double precision :: delta_e_ab(N_states)
       double precision :: hib_test,hja_test,hab_test


       do jdet = 1, idx(0)
        if(idx(jdet).ne.idet)then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CASE OF THE MONO EXCITATIONS 
         if(degree(jdet) == 1)then
          ! two determinants | Idet > and | Jdet > which are connected throw a mono excitation operator 
          ! are connected by the presence of the perturbers determinants |det_tmp>
           aorb  = index_orb_act_mono(idx(jdet),1)   !  a_{aorb}
           borb  = index_orb_act_mono(idx(jdet),2)   !  a^{\dagger}_{borb}
           kspin = index_orb_act_mono(idx(jdet),3)   !  spin of the excitation 
           ! the determinants Idet and Jdet interact throw the following operator 
           !      | Jdet > = a^{\dagger}_{borb,kspin} a_{aorb, kspin} | Idet >
  
           do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                            !  a^{\dagger}_r a_{i} (ispin)
             if(ispin == kspin .and. vorb.le.rorb)cycle ! condition not to double count 
             do jspin = 1, 2 
               if (jspin .ne. kspin)then
                do corb = 1, n_act_orb
                 if(perturb_dets_phase(corb,jspin,ispin).le.-100d0)cycle
                 ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{corb,kspin} a_{iorb,ispin} | Idet > 
                 do inint = 1, N_int
                  det_tmp(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                  det_tmp_bis(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp_bis(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                 enddo
                 ! < idet | H | det_tmp > = phase * (ir|cv)
                 call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
                 if(ispin == jspin)then
                  hib= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else
                  hib= phase * active_int(corb,1) 
                 endif
                 
                 ! | det_tmp_bis > = a^{\dagger}_{borb,kspin} a_{aorb,kspin} | det_tmp > 
                 call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),kspin,i_ok)
                 if(i_ok .ne. 1)cycle
                 call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)

                 ! < det_tmp     | H | det_tmp_bis >  = F_{aorb,borb}
                 hab = (fock_operator_local(aorb,borb,kspin) ) * phase  
                 ! < jdet | H | det_tmp_bis > = phase * (ir|cv)
                 call get_double_excitation(det_tmp_bis,psi_ref(1,1,idx(jdet)),exc,phase,N_int)
                 if(ispin == jspin)then
                  hja= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else 
                  hja= phase * (active_int(corb,1))
                 endif
                 do istate = 1, N_states
                  delta_e_ab(istate) = delta_e(corb,jspin,istate) + one_anhil_one_creat(borb,aorb,kspin,kspin,istate)
                  matrix_1h2p(idx(jdet),idet,istate) = matrix_1h2p(idx(jdet),idet,istate) + & 
                                       hib / delta_e(corb,jspin,istate) * hab / delta_e_ab(istate) * hja  
                                  !   < det_tmp     | H | Idet >         / delta_E (Idet --> det_tmp )
                                  !   < det_tmp     | H | det_tmp_bis >  / delta_E (Idet --> det_tmp --> det_tmp_bis)
                                  !   < det_tmp_bis | H | Jdet > 
                 enddo
                enddo ! corb
              else 
                do corb = 1, n_act_orb
                 if(corb == aorb .or. corb == borb) cycle
                 if(perturb_dets_phase(corb,jspin,ispin).le.-100d0)cycle
                 ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{corb,jspin} a_{iorb,ispin} | Idet > 
                 do inint = 1, N_int
                  det_tmp(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                  det_tmp_bis(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp_bis(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                 enddo
                 ! < idet | H | det_tmp > = phase * ( (ir|cv) - (iv|cr) )
                 call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
                 if(ispin == jspin)then
                  hib= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else
                  hib= phase * active_int(corb,1) 
                 endif
                 ! | det_tmp_bis > = a^{\dagger}_{borb,kspin} a_{aorb,kspin} | det_tmp > 
                 call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),kspin,i_ok)
                 if(i_ok .ne. 1)cycle
                 call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
!                ! < det_tmp | H | det_tmp_bis >  = F_{aorb,borb}
                 hab = fock_operator_local(aorb,borb,kspin) * phase  
                 ! < jdet | H | det_tmp_bis > = phase * ( (ir|cv) - (iv|cr) )
                 call get_double_excitation(det_tmp_bis,psi_ref(1,1,idx(jdet)),exc,phase,N_int)
                 if(ispin == jspin)then
                  hja= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else 
                  hja= phase * (active_int(corb,1))
                 endif
                 do istate = 1, N_states
                  delta_e_ab(istate) = delta_e(corb,jspin,istate) + one_anhil_one_creat(borb,aorb,kspin,kspin,istate)
                  matrix_1h2p(idx(jdet),idet,istate) = matrix_1h2p(idx(jdet),idet,istate) + & 
                                       hib / delta_e(corb,jspin,istate) * hab / delta_e_ab(istate) * hja  
                                  !   < det_tmp     | H | Idet >         / delta_E (Idet --> det_tmp )
                                  !   < det_tmp     | H | det_tmp_bis >  / delta_E (Idet --> det_tmp --> det_tmp_bis)
                                  !   < det_tmp_bis | H | Jdet > 
                 enddo
                enddo ! corb
              
              endif
             enddo ! jspin 
           enddo ! ispin 
         else     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Case of double excitations !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            !  a^{\dagger}_r a_{i} (ispin)
          aorb = index_orb_act_mono(idx(jdet),4) ! hole of a beta electron 
          borb = index_orb_act_mono(idx(jdet),5) ! propagation of the hole :: mono excitation of alpha spin
          do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                            !  a^{\dagger}_r a_{i} (ispin)
         !  ! first combination of spin :: | det_tmp > = a_{aorb,beta} | Idet >
            jspin = 2
            if(perturb_dets_phase(aorb,jspin,ispin).le.-100d0)cycle
            do inint = 1, N_int
             det_tmp(inint,1)  = perturb_dets(inint,1,aorb,jspin,ispin) 
             det_tmp(inint,2)  = perturb_dets(inint,2,aorb,jspin,ispin) 
             det_tmp_bis(inint,1)  = perturb_dets(inint,1,aorb,jspin,ispin) 
             det_tmp_bis(inint,2)  = perturb_dets(inint,2,aorb,jspin,ispin) 
            enddo
            call get_double_excitation(det_tmp,psi_ref(1,1,idet),exc,phase,N_int)
            if(ispin == jspin)then
             hib= phase * (active_int(borb,1) - active_int(borb,2)) 
            else 
             hib= phase * (active_int(borb,1))
            endif
            if(      index_orb_act_mono(idx(jdet),1) == index_orb_act_mono(idx(jdet),5))then
             call do_mono_excitation(det_tmp_bis,list_act(borb),list_act(aorb),1,i_ok)
             if(i_ok .ne. 1)then
              call debug_det(psi_ref(1,1,idet),N_int)
              call debug_det(psi_ref(1,1,idx(jdet)),N_int)
              print*, aorb, borb
              call debug_det(det_tmp,N_int)
              stop
             endif
            else 
             call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),1,i_ok)
            endif
             
            if(i_ok .ne. 1)cycle
            call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
            ! < det_tmp     | H | det_tmp_bis >  = F_{aorb,borb}
            if (aorb == borb)then
             print*, 'iahaha'
             stop
            endif
            hab = fock_operator_local(aorb,borb,1) * phase  
            call get_double_excitation(det_tmp_bis,psi_ref(1,1,idx(jdet)),exc,phase,N_int)
            if(ispin == jspin)then
             hja= phase * (active_int(borb,1) - active_int(borb,2)) 
            else 
             hja= phase * (active_int(borb,1))
            endif
            do istate = 1, N_states
             delta_e_ab(istate) = delta_e(aorb,jspin,istate) + one_anhil_one_creat(borb,aorb,1,1,istate)
             matrix_1h2p(idx(jdet),idet,istate) = matrix_1h2p(idx(jdet),idet,istate) + & 
                                  hib / delta_e(aorb,jspin,istate) * hab / delta_e_ab(istate) * hja  
                             !   < det_tmp     | H | Idet >         / delta_E (Idet --> det_tmp )
                             !   < det_tmp     | H | det_tmp_bis >  / delta_E (Idet --> det_tmp --> det_tmp_bis)
                             !   < det_tmp_bis | H | Jdet > 
            enddo  !! istate

         !  ! second combination of spin :: | det_tmp > = a_{aorb,alpha} | Idet >
            jspin = 1
            if(perturb_dets_phase(aorb,jspin,ispin).le.-100d0)cycle
            do inint = 1, N_int
             det_tmp(inint,1)  = perturb_dets(inint,1,aorb,jspin,ispin) 
             det_tmp(inint,2)  = perturb_dets(inint,2,aorb,jspin,ispin) 
             det_tmp_bis(inint,1)  = perturb_dets(inint,1,aorb,jspin,ispin) 
             det_tmp_bis(inint,2)  = perturb_dets(inint,2,aorb,jspin,ispin) 
            enddo
            call get_double_excitation(det_tmp,psi_ref(1,1,idet),exc,phase,N_int)
            if(ispin == jspin)then
             hib= phase * (active_int(borb,1) - active_int(borb,2)) 
            else 
             hib= phase * (active_int(borb,1))
            endif
            if(      index_orb_act_mono(idx(jdet),1) == index_orb_act_mono(idx(jdet),5))then
             call do_mono_excitation(det_tmp_bis,list_act(borb),list_act(aorb),2,i_ok)
             if(i_ok .ne. 1)then
              call debug_det(psi_ref(1,1,idet),N_int)
              call debug_det(psi_ref(1,1,idx(jdet)),N_int)
              print*, aorb, borb
              call debug_det(det_tmp,N_int)
              stop
             endif
            else 
             call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),2,i_ok)
            endif
             
            if(i_ok .ne. 1)cycle
            call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
            ! < det_tmp     | H | det_tmp_bis >  = F_{aorb,borb}
            hab = fock_operator_local(aorb,borb,2) * phase  
            call get_double_excitation(det_tmp_bis,psi_ref(1,1,idx(jdet)),exc,phase,N_int)
            if(ispin == jspin)then
             hja= phase * (active_int(borb,1) - active_int(borb,2)) 
            else 
             hja= phase * (active_int(borb,1))
            endif
            do istate = 1, N_states
             delta_e_ab(istate) = delta_e(aorb,jspin,istate) + one_anhil_one_creat(borb,aorb,1,1,istate)
             matrix_1h2p(idx(jdet),idet,istate) = matrix_1h2p(idx(jdet),idet,istate) + & 
                                  hib / delta_e(aorb,jspin,istate) * hab / delta_e_ab(istate) * hja  
                             !   < det_tmp     | H | Idet >         / delta_E (Idet --> det_tmp )
                             !   < det_tmp     | H | det_tmp_bis >  / delta_E (Idet --> det_tmp --> det_tmp_bis)
                             !   < det_tmp_bis | H | Jdet > 
            enddo  !! istate
          enddo  !! ispin

    
         endif   !!  en of test if jdet is a single or a double excitation of type K_ab

        else   !! jdet is idet 
        ! diagonal part of the dressing : interaction of | Idet > with all the perturbers generated by the excitations 
        ! 
        ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{aorb,kspin} a_{iorb,ispin} | Idet > 
         do ispin = 1, 2
          do kspin = 1, 2
           do a = 1, n_act_orb      ! First active 
           if( perturb_dets_phase(a,kspin,ispin) .le. -10.d0)cycle
            if(ispin == kspin .and. vorb.le.rorb)cycle ! condition not to double count 
            contrib_hij = perturb_dets_hij(a,kspin,ispin) * perturb_dets_hij(a,kspin,ispin)
            do istate = 1, N_states
!            matrix_1h2p(idet,idet,istate) += contrib_hij * delta_e(a,kspin,istate)
!            perturb_dets_hpsi0(a,kspin,ispin,istate) += psi_coef(idet,istate) * perturb_dets_hij(a,kspin,ispin)
!            coef_perturb_from_idet(a,kspin,ispin,istate,1) += psi_coef(idet,istate) & 
!                                                            * perturb_dets_hij(a,kspin,ispin) * delta_e(a,kspin,istate)
            enddo
           enddo
          enddo
         enddo
        
        endif
        
       enddo !! jdet 

      
      enddo
    enddo
   enddo
 enddo





end

