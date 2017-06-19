
subroutine give_2p_new(matrix_2p)
  use bitmasks
 implicit none
 double precision , intent(inout) :: matrix_2p(N_det,N_det,*)
 integer :: i,v,r,a,b,c
 integer :: iorb, vorb, rorb, aorb, borb,corb
 integer :: ispin,jspin
 integer :: idet,jdet
 integer(bit_kind) :: perturb_dets(N_int,2,n_act_orb,n_act_orb,2,2)
 double precision  :: perturb_dets_phase(n_act_orb,n_act_orb,2,2)
 double precision  :: perturb_dets_hij(n_act_orb,n_act_orb,2,2)
 double precision  :: perturb_dets_hpsi0(n_act_orb,n_act_orb,2,2,N_states)
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer(bit_kind) :: det_tmp_j(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,n_act_orb,2)
 double precision :: hij,phase
 double precision :: accu_contrib(N_states)
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 double precision :: delta_e(n_act_orb,n_act_orb,2,2,N_states)
 double precision :: delta_e_inv(n_act_orb,n_act_orb,2,2,N_states)
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
!matrix_2p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_ref(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_ref(inint,2,1))
 enddo
   do v = 1, n_virt_orb  ! First virtual 
    vorb = list_virt(v)
    do r = 1, n_virt_orb    ! Second virtual
     rorb = list_virt(r) 
     ! take all the integral you will need for r,v fixed
     do a = 1, n_act_orb
      aorb = list_act(a)
      do b = 1, n_act_orb
       borb = list_act(b)
       active_int(a,b,1) = get_mo_bielec_integral(aorb,borb,rorb,vorb,mo_integrals_map) ! direct   ( a--> r | b--> v )
       active_int(a,b,2) = get_mo_bielec_integral(aorb,borb,vorb,rorb,mo_integrals_map) ! exchange ( b--> r | a--> v )
       perturb_dets_phase(a,b,1,1) = -1000.d0
       perturb_dets_phase(a,b,1,2) = -1000.d0
       perturb_dets_phase(a,b,2,2) = -1000.d0
       perturb_dets_phase(a,b,2,1) = -1000.d0
       perturb_dets_phase(b,a,1,1) = -1000.d0
       perturb_dets_phase(b,a,1,2) = -1000.d0
       perturb_dets_phase(b,a,2,2) = -1000.d0
       perturb_dets_phase(b,a,2,1) = -1000.d0
      enddo
     enddo
     

      do istate = 1, N_states
       delta_e_inactive_virt(istate) = &
                                    - fock_virt_total_spin_trace(rorb,istate)               & 
                                    - fock_virt_total_spin_trace(vorb,istate)                 
      enddo
      do idet = 1, N_det
!       call get_excitation_degree_vector_mono(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
        call get_excitation_degree_vector(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
        do ispin = 1, 2  ! spin of the couple a-a^dagger (aorb,rorb)
         do jspin = 1, 2   ! spin of the couple a-a^dagger (borb,vorb)
           do b = 1, n_act_orb      ! First active 
            borb = list_act(b)
            do a = 1, n_act_orb      ! First active 
             aorb = list_act(a)
!            if(ispin == 2.and. jspin ==1)then
!             perturb_dets_phase(a,b,ispin,jspin) = -1000.0d0
!             perturb_dets_hij(a,b,ispin,jspin) = 0.d0
!             cycle ! condition not to double count 
!            endif

             if(ispin == jspin .and. vorb.le.rorb)then 
              perturb_dets_phase(a,b,ispin,jspin) = -1000.0d0
              perturb_dets_hij(a,b,ispin,jspin) = 0.d0
              cycle ! condition not to double count 
             endif
             if(ispin == jspin .and. aorb.le.borb) then 
              perturb_dets_phase(a,b,ispin,jspin) = -1000.0d0
              perturb_dets_hij(a,b,ispin,jspin) = 0.d0
              cycle ! condition not to double count 
             endif
             do inint = 1, N_int
              det_tmp(inint,1) = psi_ref(inint,1,idet)
              det_tmp(inint,2) = psi_ref(inint,2,idet)
             enddo
            ! Do the excitation  (aorb,ispin) --> (rorb,ispin)
            call clear_bit_to_integer(aorb,det_tmp(1,ispin),N_int)  ! hole in "aorb" of spin Ispin
            call set_bit_to_integer(rorb,det_tmp(1,ispin),N_int)    ! particle in "rorb" of spin Ispin
            
            ! Do the excitation  (borb,jspin) --> (vorb,jspin)
            call clear_bit_to_integer(borb,det_tmp(1,jspin),N_int)  ! hole in "borb" of spin Jspin
            call set_bit_to_integer(vorb,det_tmp(1,jspin),N_int)    ! particle in "vorb" of spin Jspin
            
            ! Check if the excitation is possible or not on psi_ref(idet)
            accu_elec= 0
            do inint = 1, N_int
             accu_elec+= popcnt(det_tmp(inint,1)) + popcnt(det_tmp(inint,2))
            enddo
            if(accu_elec .ne. elec_num_tab_local(2)+elec_num_tab_local(1))then
             perturb_dets_phase(a,b,ispin,jspin) = -1000.0d0
             perturb_dets_hij(a,b,ispin,jspin) = 0.d0
             cycle
            endif
            do inint = 1, N_int
             perturb_dets(inint,1,a,b,ispin,jspin) = det_tmp(inint,1) 
             perturb_dets(inint,2,a,b,ispin,jspin) = det_tmp(inint,2) 
            enddo
             
            call get_double_excitation(psi_ref(1,1,idet),det_tmp,exc,phase,N_int)
            perturb_dets_phase(a,b,ispin,jspin) = phase
            
            do istate = 1, N_states
             delta_e(a,b,ispin,jspin,istate) = two_anhil(a,b,ispin,jspin,istate)   + delta_e_inactive_virt(istate)
             delta_e_inv(a,b,ispin,jspin,istate) = 1.d0 / delta_e(a,b,ispin,jspin,istate)
            enddo
            if(ispin == jspin)then
             perturb_dets_hij(a,b,ispin,jspin) = phase * (active_int(a,b,2) - active_int(a,b,1) )
            else 
             perturb_dets_hij(a,b,ispin,jspin) = phase * active_int(a,b,1) 
            endif
            call i_H_j(psi_ref(1,1,idet),det_tmp,N_int,hij)
            if(hij.ne.perturb_dets_hij(a,b,ispin,jspin))then
             print*, active_int(a,b,1) , active_int(b,a,1) 
             double precision :: hmono,hdouble
             call i_H_j_verbose(psi_ref(1,1,idet),det_tmp,N_int,hij,hmono,hdouble)
             print*,  'pb !! hij.ne.perturb_dets_hij(a,b,ispin,jspin)'
             print*, ispin,jspin
             print*, aorb,rorb,borb,vorb
             print*, hij,perturb_dets_hij(a,b,ispin,jspin)
             call debug_det(psi_ref(1,1,idet),N_int)
             call debug_det(det_tmp,N_int)
             stop
            endif
           enddo ! b 
         enddo ! a 
        enddo ! jspin 
       enddo ! ispin

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
           if (exc(0,1,1) == 1) then
             ! Mono alpha
             index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,1))   !!!  a_a ALPHA 
             index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b} ALPHA 
             index_orb_act_mono(idx(jdet),3) = 1
             ! Mono beta
             index_orb_act_mono(idx(jdet),4) = list_act_reverse(exc(1,1,2))   !!!  a_a BETA 
             index_orb_act_mono(idx(jdet),5) = list_act_reverse(exc(1,2,2))   !!!  a^{\dagger}_{b} BETA 
             index_orb_act_mono(idx(jdet),6) = 2
           else if (exc(0,1,1) == 2) then
             index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,1))   !!!  a_a ALPHA 
             index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b} ALPHA 
             index_orb_act_mono(idx(jdet),3) = 1
             index_orb_act_mono(idx(jdet),4) = list_act_reverse(exc(2,1,1))   !!!  a_c ALPHA 
             index_orb_act_mono(idx(jdet),5) = list_act_reverse(exc(2,2,1))   !!!  a^{\dagger}_{d} ALPHA
             index_orb_act_mono(idx(jdet),6) = 1
           else if (exc(0,1,2) == 2) then
             index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,2))   !!!  a_a BETA  
             index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(2,1,2))   !!!  a^{\dagger}_{b} BETA  
             index_orb_act_mono(idx(jdet),3) = 2
             index_orb_act_mono(idx(jdet),4) = list_act_reverse(exc(1,2,2))   !!!  a_c BETA 
             index_orb_act_mono(idx(jdet),5) = list_act_reverse(exc(2,2,2))   !!!  a^{\dagger}_{d} BETA 
             index_orb_act_mono(idx(jdet),6) = 2
           endif
          endif
       enddo



!      do jdet = 1, idx(0)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CASE OF THE MONO EXCITATIONS 
!       if(degree(jdet) == 1)then
!         ! two determinants | Idet > and | Jdet > which are connected throw a mono excitation operator 
!         ! are connected by the presence of the perturbers determinants |det_tmp>
!          aorb  = index_orb_act_mono(idx(jdet),1)   !  a_{aorb}
!          borb  = index_orb_act_mono(idx(jdet),2)   !  a^{\dagger}_{borb}
!          kspin = index_orb_act_mono(idx(jdet),3)   !  spin of the excitation 
!          ! the determinants Idet and Jdet interact throw the following operator 
!          !      | Jdet > = a^{\dagger}_{borb,kspin} a_{aorb, kspin} | Idet >
  
!          accu_contrib = 0.d0
           do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                            !  a^{\dagger}_r a_{a} (ispin)
               !!!! SECOND ORDER CONTRIBTIONS 
             ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,jspin} a_{corb,jspin} a_{iorb,ispin} | Idet > 
              do jspin = 1, 2
               if(ispin == 2 .and. jspin ==1)cycle
               do b = 1, n_act_orb
                do a = 1, n_act_orb
                 logical :: cycle_same_spin_second_order(2)
                 cycle_same_spin_second_order(1) = .False.
                 cycle_same_spin_second_order(2) = .False.
                 if(perturb_dets_phase(a,b,ispin,jspin).le.-10d0)cycle
                 if(ispin == jspin .and. vorb.le.rorb)then
                  cycle_same_spin_second_order(1) = .True. 
                 endif
                 if(ispin == jspin .and. aorb.le.borb)then
                  cycle_same_spin_second_order(2) = .True. 
                 endif
                  do inint = 1, N_int
                   det_tmp(inint,1) = perturb_dets(inint,1,a,b,ispin,jspin) 
                   det_tmp(inint,2) = perturb_dets(inint,2,a,b,ispin,jspin) 
                  enddo
                  do jdet = 1, idx(0)
!                  if(idx(jdet).gt.idet)cycle
                   do istate = 1, N_states
                    call i_H_j(psi_ref(1,1,idx(jdet)),det_tmp,N_int,hij)
                    matrix_2p(idx(jdet),idet,istate) += hij * perturb_dets_hij(a,b,ispin,jspin) * delta_e_inv(a,b,ispin,jspin,istate)
                   enddo
                  enddo ! jdet
                enddo ! b
               enddo ! a
              enddo ! jspin
           enddo ! ispin 

!       else if (degree(jdet) == 0)then
!       
!       endif 
!      enddo !! jdet 

      
      enddo
    enddo
   enddo
end
