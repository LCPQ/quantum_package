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
 double precision :: get_mo_bielec_integral_schwartz
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
!matrix_2h1p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_det(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_det(inint,2,1))
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
      active_int(a,1) = get_mo_bielec_integral_schwartz(iorb,jorb,rorb,aorb,mo_integrals_map) ! direct
      active_int(a,2) = get_mo_bielec_integral_schwartz(iorb,jorb,aorb,rorb,mo_integrals_map) ! exchange
      perturb_dets_phase(a,1,1) = -1000.d0
      perturb_dets_phase(a,1,2) = -1000.d0
      perturb_dets_phase(a,2,2) = -1000.d0
      perturb_dets_phase(a,2,1) = -1000.d0
     enddo
     
  integer           :: degree(N_det)
  integer           :: idx(0:N_det)
  double precision :: delta_e(n_act_orb,2,N_states)
  integer :: istate
  integer :: index_orb_act_mono(N_det,3)

      do idet = 1, N_det
        call get_excitation_degree_vector_mono(psi_det,psi_det(1,1,idet),degree,N_int,N_det,idx)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
        do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
         do jspin = 1, 2   ! spin of the couple z-a^dagger (j,a)
          if(ispin == jspin .and. iorb.le.jorb)cycle ! condition not to double count 
           do a = 1, n_act_orb      ! First active 
            aorb = list_act(a)
            do inint = 1, N_int
             det_tmp(inint,1) = psi_det(inint,1,idet)
             det_tmp(inint,2) = psi_det(inint,2,idet)
            enddo
           ! Do the excitation  inactive -- > virtual
           call clear_bit_to_integer(iorb,det_tmp(1,ispin),N_int)  ! hole in "iorb" of spin Ispin
           call set_bit_to_integer(rorb,det_tmp(1,ispin),N_int)    ! particle in "rorb" of spin Ispin

           ! Do the excitation  inactive -- > active 
           call clear_bit_to_integer(jorb,det_tmp(1,jspin),N_int)  ! hole in "jorb" of spin Jspin
           call set_bit_to_integer(aorb,det_tmp(1,jspin),N_int) ! particle in "aorb" of spin Jspin

           ! Check if the excitation is possible or not on psi_det(idet)
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
           call get_double_excitation(psi_det(1,1,idet),det_tmp,exc,phase,N_int)
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!! Second order coefficient :  interactions between the perturbers throw the active space
       do a = 1, n_act_orb
        do jspin = 1, 2
         do ispin = 1, 2
          if( perturb_dets_phase(a,jspin,ispin) .le. -10.d0)cycle
          ! determinant perturber | det_tmp > = a^{\dagger}_{r,ispin} a^{\dagger}_{v,jspin} a_{a,jspin} a_{i,ispin} | Idet >
          do inint = 1, N_int
           det_tmp(inint,1)  = iand(perturb_dets(inint,1,a,jspin,ispin),cas_bitmask(inint,1,1)) 
           det_tmp(inint,2)  = iand(perturb_dets(inint,2,a,jspin,ispin),cas_bitmask(inint,1,1)) 
          enddo
           do istate = 1, N_states
            coef_perturb_from_idet(a,jspin,ispin,istate,2) = 0.d0
           enddo
           do b = 1, n_act_orb
            do kspin = jspin , jspin
             integer :: degree_scalar
              if( perturb_dets_phase(b,kspin,ispin) .le. -10.d0)cycle
            do inint = 1, N_int
             det_tmp_j(inint,1)  = iand(perturb_dets(inint,1,b,kspin,ispin),cas_bitmask(inint,1,1)) 
             det_tmp_j(inint,2)  = iand(perturb_dets(inint,2,b,kspin,ispin),cas_bitmask(inint,1,1)) 
            enddo
              call get_excitation_degree(det_tmp,det_tmp_j,degree_scalar,N_int)  
            if (degree_scalar > 2 .or. degree_scalar == 0)cycle
              ! determinant perturber | det_tmp_j > = a^{\dagger}_{r,ispin} a^{\dagger}_{v,jspin} a_{b,jspin} a_{i,ispin} | Idet >
!           print*, '**********************'
!           integer(bit_kind) :: det_bis(N_int,2)
!           call debug_det(det_tmp,N_int)
!           call debug_det(det_tmp_j,N_int)
!           do inint = 1, N_int
!            det_bis(inint,1)  = perturb_dets(inint,1,b,kspin,ispin)
!            det_bis(inint,2)  = perturb_dets(inint,2,b,kspin,ispin)
!           enddo
!           call debug_det(det_bis,N_int)
            call i_H_j_dyall(det_tmp,det_tmp_j,N_int,hij)
            do istate = 1, N_states
             coef_perturb_from_idet(a,jspin,ispin,istate,2) += coef_perturb_from_idet(b,kspin,ispin,istate,1) & 
                                                             * hij / delta_e(a,jspin,istate)
              if(dabs(hij).gt.0.01d0)then
               print*,degree_scalar, hij
               print*, coef_perturb_from_idet(b,kspin,ispin,istate,1)* hij / delta_e(a,jspin,istate),coef_perturb_from_idet(a,jspin,ispin,istate,1)
                
              endif
            enddo
           enddo
          enddo
         enddo
        enddo
       enddo
       do a = 1, n_act_orb
        do jspin = 1, 2
         do ispin = 1, 2
          if( perturb_dets_phase(a,jspin,ispin) .le. -10.d0)cycle
            do istate = 1, N_states
!            print*, coef_perturb_from_idet(a,jspin,ispin,istate,1),coef_perturb_from_idet(a,jspin,ispin,istate,2)
             coef_perturb_from_idet(a,jspin,ispin,istate,2) += coef_perturb_from_idet(a,jspin,ispin,istate,1)  
            enddo
          enddo
         enddo
        enddo
!      stop
!!!!!!!!!!!!!!!!!!!!!!!!!!! determination of the connections between I and the other J determinants mono excited in the CAS
!!!!!!!!!!!!!!!!!!!!!!!!!!!! the determinants I and J must be connected by the following operator 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | a_{b} a^{\dagger}_a | Idet> 
       do jdet = 1, idx(0)
         if(idx(jdet).ne.idet)then
          call get_mono_excitation(psi_det(1,1,idet),psi_det(1,1,idx(jdet)),exc,phase,N_int)
          if (exc(0,1,1) == 1) then
            ! Mono alpha
            index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,2,1))   !!! a^{\dagger}_a 
            index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,1,1))   !!! a_{b}
            index_orb_act_mono(idx(jdet),3) = 1
          else
            ! Mono beta
            index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,2,2))   !!!  a^{\dagger}_a
            index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,1,2))   !!!  a_{b}
            index_orb_act_mono(idx(jdet),3) = 2
          endif
         else
          index_orb_act_mono(idx(jdet),1) = -1
         endif
       enddo

       integer :: kspin
       do jdet = 1, idx(0)
        if(idx(jdet).ne.idet)then
        ! two determinants | Idet > and | Jdet > which are connected throw a mono excitation operator 
        ! are connected by the presence of the perturbers determinants |det_tmp>
         aorb  = index_orb_act_mono(idx(jdet),1)   !  a^{\dagger}_{aorb}
         borb  = index_orb_act_mono(idx(jdet),2)   !  a_{borb}
         kspin = index_orb_act_mono(idx(jdet),3)   !  spin of the excitation 
         ! the determinants Idet and Jdet interact throw the following operator 
         !      | Jdet > = a_{borb,kspin} a^{\dagger}_{aorb, kspin} | Idet >

         do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                          !  a^{\dagger}_r a_{i} (ispin)
          if(ispin == kspin .and. iorb.le.jorb)cycle ! condition not to double count 

          ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{aorb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Idet > 
          do inint = 1, N_int
           det_tmp(inint,1)  = perturb_dets(inint,1,aorb,kspin,ispin) 
           det_tmp(inint,2)  = perturb_dets(inint,2,aorb,kspin,ispin) 
          enddo
          double precision :: hja
          ! you determine the interaction between the excited determinant and the other parent | Jdet >
          ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{borb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Jdet >
          ! hja = < det_tmp | H | Jdet >
          call get_double_excitation(psi_det(1,1,idx(jdet)),det_tmp,exc,phase,N_int)
          if(kspin == ispin)then
           hja = phase * (active_int(borb,2) - active_int(borb,1) )
          else
           hja = phase * active_int(borb,1)  
          endif

          do istate = 1, N_states
           matrix_2h1p(idx(jdet),idet,istate) += hja * coef_perturb_from_idet(aorb,kspin,ispin,istate,2)
          enddo
         enddo ! ispin 

        else
        ! diagonal part of the dressing : interaction of | Idet > with all the perturbers generated by the excitations 
        ! 
        ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{aorb,kspin} a_{jorb,kspin} a_{iorb,ispin} | Idet > 
         do ispin = 1, 2
          do kspin = 1, 2
           if(ispin == kspin .and. iorb.le.jorb)cycle ! condition not to double count 
           do a = 1, n_act_orb      ! First active 
            do istate = 1, N_states
             matrix_2h1p(idet,idet,istate) += coef_perturb_from_idet(a,kspin,ispin,istate,2) * perturb_dets_hij(a,kspin,ispin) 
            enddo
           enddo
          enddo
         enddo
        
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
 integer :: i,v,r,a,b
 integer :: iorb, vorb, rorb, aorb, borb
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
 double precision :: get_mo_bielec_integral_schwartz
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
 accu_contrib = 0.d0
!matrix_1h2p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_det(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_det(inint,2,1))
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
      active_int(a,1) = get_mo_bielec_integral_schwartz(iorb,aorb,rorb,vorb,mo_integrals_map) ! direct
      active_int(a,2) = get_mo_bielec_integral_schwartz(iorb,aorb,vorb,rorb,mo_integrals_map) ! exchange
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
        call get_excitation_degree_vector_mono_or_exchange(psi_det,psi_det(1,1,idet),degree,N_int,N_det,idx)
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
             det_tmp(inint,1) = psi_det(inint,1,idet)
             det_tmp(inint,2) = psi_det(inint,2,idet)
            enddo
           ! Do the excitation  inactive -- > virtual
           call clear_bit_to_integer(iorb,det_tmp(1,ispin),N_int)  ! hole in "iorb" of spin Ispin
           call set_bit_to_integer(rorb,det_tmp(1,ispin),N_int)    ! particle in "rorb" of spin Ispin

           ! Do the excitation  active -- > virtual 
           call clear_bit_to_integer(aorb,det_tmp(1,jspin),N_int)  ! hole in "aorb" of spin Jspin
           call set_bit_to_integer(vorb,det_tmp(1,jspin),N_int)    ! particle in "vorb" of spin Jspin

           ! Check if the excitation is possible or not on psi_det(idet)
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
            
           call get_double_excitation(psi_det(1,1,idet),det_tmp,exc,phase,N_int)
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
             integer :: i_hole,i_part
       do jdet = 1, idx(0)
         if(idx(jdet).ne.idet)then
!         print*, degree(jdet)
          if(degree(jdet)==1)then
           call get_mono_excitation(psi_det(1,1,idet),psi_det(1,1,idx(jdet)),exc,phase,N_int)
           if (exc(0,1,1) == 1) then
             ! Mono alpha
             i_hole = list_act_reverse(exc(1,1,1))   !!!  a_a
             i_part = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b}
             kspin = 1                               !!!  kspin 
             index_orb_act_mono(idx(jdet),1) = i_hole
             index_orb_act_mono(idx(jdet),2) = i_part 
             index_orb_act_mono(idx(jdet),3) = kspin                          
             call i_H_j_dyall(psi_det(1,1,idet),psi_det(1,1,idx(jdet)),N_int,hij)
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
             call i_H_j_dyall(psi_det(1,1,idet),psi_det(1,1,idx(jdet)),N_int,hij)
             fock_operator_local(i_hole,i_part,kspin) = hij * phase ! phase less fock operator 
             fock_operator_local(i_part,i_hole,kspin) = hij * phase ! phase less fock operator 
           endif
          else if(degree(jdet)==2)then
           call get_double_excitation(psi_det(1,1,idet),psi_det(1,1,idx(jdet)),exc,phase,N_int)
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



       do jdet = 1, idx(0)
        if(idx(jdet).ne.idet)then
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
             integer ::corb,dorb,i_ok
             integer(bit_kind) :: det_tmp_bis(N_int,2)
             double precision :: hib , hab 
             double precision :: delta_e_ab(N_states)
             double precision :: hib_test,hja_test,hab_test
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
!                call i_H_j(det_tmp,psi_det(1,1,idet),N_int,hib)
                 call get_double_excitation(psi_det(1,1,idet),det_tmp,exc,phase,N_int)
                 if(ispin == jspin)then
                  hib= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else
                  hib= phase * active_int(corb,1) 
                 endif
!                if(hib_test .ne. hib)then
!                 print*, 'hib_test .ne. hib'
!                 print*, hib, hib_test 
!                 stop 
!                endif
                 
                 ! | det_tmp_bis > = a^{\dagger}_{borb,kspin} a_{aorb,kspin} | det_tmp > 
                 call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),kspin,i_ok)
                 if(i_ok .ne. 1)cycle
                 call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)

                 ! < det_tmp     | H | det_tmp_bis >  = F_{aorb,borb}
!                call i_H_j(det_tmp_bis,det_tmp,N_int,hab)
                 hab = fock_operator_local(aorb,borb,kspin) * phase  
                 ! < jdet | H | det_tmp_bis > = phase * (ir|cv)
!                call i_H_j(det_tmp_bis,psi_det(1,1,idx(jdet)),N_int,hja)
                 call get_double_excitation(det_tmp_bis,psi_det(1,1,idx(jdet)),exc,phase,N_int)
                 if(ispin == jspin)then
                  hja= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else 
                  hja= phase * (active_int(corb,1))
                 endif
!                if(hja_test .ne. hja)then
!                 print*, 'hja_test .ne. hja'
!                 print*, hja, hja_test 
!                 stop 
!                endif
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
                 ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{corb,kspin} a_{iorb,ispin} | Idet > 
                 do inint = 1, N_int
                  det_tmp(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                  det_tmp_bis(inint,1)  = perturb_dets(inint,1,corb,jspin,ispin) 
                  det_tmp_bis(inint,2)  = perturb_dets(inint,2,corb,jspin,ispin) 
                 enddo
                 ! < idet | H | det_tmp > = phase * ( (ir|cv) - (iv|cr) )
!                call i_H_j(det_tmp,psi_det(1,1,idet),N_int,hib)
                 call get_double_excitation(psi_det(1,1,idet),det_tmp,exc,phase,N_int)
                 if(ispin == jspin)then
                  hib= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else
                  hib= phase * active_int(corb,1) 
                 endif
!                if(hib_test .ne. hib)then
!                 print*, 'hib_test .ne. hib jspin == kspin'
!                 print*, hib, hib_test 
!                 stop 
!                endif
                 ! | det_tmp_bis > = a^{\dagger}_{borb,kspin} a_{aorb,kspin} | det_tmp > 
                 call do_mono_excitation(det_tmp_bis,list_act(aorb),list_act(borb),kspin,i_ok)
                 if(i_ok .ne. 1)cycle
                 call get_mono_excitation(det_tmp,det_tmp_bis,exc,phase,N_int)
!                ! < det_tmp | H | det_tmp_bis >  = F_{aorb,borb}
                 hab = fock_operator_local(aorb,borb,kspin) * phase  
!                call i_H_j(det_tmp_bis,det_tmp,N_int,hab)
                 ! < jdet | H | det_tmp_bis > = phase * ( (ir|cv) - (iv|cr) )
!                call i_H_j(det_tmp_bis,psi_det(1,1,idx(jdet)),N_int,hja)
                 call get_double_excitation(det_tmp_bis,psi_det(1,1,idx(jdet)),exc,phase,N_int)
                 if(ispin == jspin)then
                  hja= phase * (active_int(corb,1) - active_int(corb,2)) 
                 else 
                  hja= phase * (active_int(corb,1))
                 endif
!                if(hja_test .ne. hja)then
!                 print*, 'hja_test .ne. hja'
!                 print*, hja, hja_test 
!                 stop 
!                endif
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
!            call debug_det(psi_det(1,1,idet),N_int)
!            call debug_det(psi_det(1,1,idx(jdet)),N_int)
!            pause

    
         endif

        else
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
        
       enddo

      
      enddo
    enddo
   enddo
 enddo
 print* , 'accu_contrib = ',accu_contrib





end







!      do a = 1, n_act_orb
!       do jspin = 1, 2
!        do ispin = 1, 2
!         if( perturb_dets_phase(a,jspin,ispin) .le. -10.d0)cycle
!         ! determinant perturber | det_tmp > = a^{\dagger}_{r,ispin} a^{\dagger}_{v,jspin} a_{a,jspin} a_{i,ispin} | Idet >
!         do inint = 1, N_int
!          det_tmp(inint,1)  = perturb_dets(inint,1,a,jspin,ispin)
!          det_tmp(inint,2)  = perturb_dets(inint,2,a,jspin,ispin)
!         enddo
!          do istate = 1, N_states
!           coef_perturb_from_idet(a,jspin,ispin,istate,2) = 0.d0
!          enddo
!          do b = 1, n_act_orb
!           do kspin = jspin , jspin
!            integer :: degree_scalar
!             if( perturb_dets_phase(b,kspin,ispin) .le. -10.d0)cycle
!           do inint = 1, N_int
!            det_tmp_j(inint,1)  = perturb_dets(inint,1,b,kspin,ispin)
!            det_tmp_j(inint,2)  = perturb_dets(inint,2,b,kspin,ispin)
!           enddo
!             call get_excitation_degree(det_tmp,det_tmp_j,degree_scalar,N_int)  
!           if (degree_scalar > 2 .or. degree_scalar == 0)cycle
!             ! determinant perturber | det_tmp_j > = a^{\dagger}_{r,ispin} a^{\dagger}_{v,jspin} a_{b,jspin} a_{i,ispin} | Idet >
!           call i_H_j(det_tmp,det_tmp_j,N_int,hij)
!           do istate = 1, N_states
!            coef_perturb_from_idet(a,jspin,ispin,istate,2) += coef_perturb_from_idet(b,kspin,ispin,istate,1) & 
!                                                            * hij / delta_e(a,jspin,istate)
!             endif
!           enddo
!          enddo
!         enddo
!        enddo
!       enddo
!      enddo



!      do a = 1, n_act_orb
!       do jspin = 1, 2
!        do ispin = 1, 2
!         if( perturb_dets_phase(a,jspin,ispin) .le. -10.d0)cycle
!         ! determinant perturber | det_tmp > = a^{\dagger}_{r,ispin} a^{\dagger}_{v,jspin} a_{a,jspin} a_{i,ispin} | Idet >
!         do inint = 1, N_int
!          det_tmp(inint,1)  = iand(perturb_dets(inint,1,a,jspin,ispin),cas_bitmask(inint,1,1)) 
!          det_tmp(inint,2)  = iand(perturb_dets(inint,2,a,jspin,ispin),cas_bitmask(inint,1,1)) 
!         enddo
!          do istate = 1, N_states
!           coef_perturb_from_idet(a,jspin,ispin,istate,2) = 0.d0
!          enddo
!          do b = 1, n_act_orb
!           do kspin = jspin , jspin
!            integer :: degree_scalar
!             if( perturb_dets_phase(b,kspin,ispin) .le. -10.d0)cycle
!           do inint = 1, N_int
!            det_tmp_j(inint,1)  = iand(perturb_dets(inint,1,b,kspin,ispin),cas_bitmask(inint,1,1)) 
!            det_tmp_j(inint,2)  = iand(perturb_dets(inint,2,b,kspin,ispin),cas_bitmask(inint,1,1)) 
!           enddo
!             call get_excitation_degree(det_tmp,det_tmp_j,degree_scalar,N_int)  
!           if (degree_scalar > 2 .or. degree_scalar == 0)cycle
!             ! determinant perturber | det_tmp_j > = a^{\dagger}_{r,ispin} a^{\dagger}_{v,jspin} a_{b,jspin} a_{i,ispin} | Idet >
!!          print*, '**********************'
!!          integer(bit_kind) :: det_bis(N_int,2)
!!          call debug_det(det_tmp,N_int)
!!          call debug_det(det_tmp_j,N_int)
!!          do inint = 1, N_int
!!           det_bis(inint,1)  = perturb_dets(inint,1,b,kspin,ispin)
!!           det_bis(inint,2)  = perturb_dets(inint,2,b,kspin,ispin)
!!          enddo
!!          call debug_det(det_bis,N_int)
!           call i_H_j_dyall(det_tmp,det_tmp_j,N_int,hij)
!           do istate = 1, N_states
!            coef_perturb_from_idet(a,jspin,ispin,istate,2) += coef_perturb_from_idet(b,kspin,ispin,istate,1) & 
!                                                            * hij / delta_e(a,jspin,istate)
!             if(dabs(hij).gt.0.01d0)then
!              print*,degree_scalar, hij
!              print*, coef_perturb_from_idet(b,kspin,ispin,istate,1)* hij / delta_e(a,jspin,istate),coef_perturb_from_idet(a,jspin,ispin,istate,1)
!               
!             endif
!           enddo
!          enddo
!         enddo
!        enddo
!       enddo
!      enddo

!      do a = 1, n_act_orb
!       do jspin = 1, 2
!        do ispin = 1, 2
!         if( perturb_dets_phase(a,jspin,ispin) .le. -10.d0)cycle
!           do istate = 1, N_states
!            coef_perturb_from_idet(a,jspin,ispin,istate,2) += coef_perturb_from_idet(a,jspin,ispin,istate,1)  
!           enddo
!         enddo
!        enddo
!       enddo
