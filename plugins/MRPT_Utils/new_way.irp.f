subroutine give_2h1p_contrib(matrix_2h1p)
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
 double precision  :: coef_perturb_from_idet(n_act_orb,2,2,N_states)
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
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
     enddo
     
  integer           :: degree(N_det)
  integer           :: idx(0:N_det)
  double precision :: delta_e(n_act_orb,2,N_states)
  integer :: istate
  integer :: index_orb_act_mono(N_det,3)

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
            perturb_dets_phase(a,jspin,ispin) = 0.0
            perturb_dets_hij(a,jspin,ispin) = 0.d0
            do istate = 1, N_states
             coef_perturb_from_idet(a,jspin,ispin,istate) = 0.d0
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
            coef_perturb_from_idet(a,jspin,ispin,istate) = perturb_dets_hij(a,jspin,ispin) / delta_e(a,jspin,istate)
           enddo

         enddo
        enddo
       enddo
       
!!!!!!!!!!!!!!!!!!!!!!!!!!! determination of the connections between I and the other J determinants mono excited in the CAS
!!!!!!!!!!!!!!!!!!!!!!!!!!!! the determinants I and J must be connected by the following operator 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | a_{b} a^{\dagger}_a | Idet> 
       do jdet = 1, idx(0)
         if(idx(jdet).ne.idet)then
          call get_mono_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
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
          call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp,exc,phase,N_int)
          if(kspin == ispin)then
           hja = phase * (active_int(borb,2) - active_int(borb,1) )
          else
           hja = phase * active_int(borb,1)  
          endif

          do istate = 1, N_states
           matrix_2h1p(idx(jdet),idet,istate) += hja * coef_perturb_from_idet(aorb,kspin,ispin,istate)
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
             matrix_2h1p(idet,idet,istate) += coef_perturb_from_idet(a,kspin,ispin,istate) * perturb_dets_hij(a,kspin,ispin) 
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


subroutine give_1h2p_contrib(matrix_1h2p)
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
 double precision  :: coef_perturb_from_idet(n_act_orb,2,2,N_states)
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
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
     enddo
     
  integer           :: degree(N_det)
  integer           :: idx(0:N_det)
  double precision :: delta_e(n_act_orb,2,N_states)
  integer :: istate
  integer :: index_orb_act_mono(N_det,3)

      do idet = 1, N_det
        call get_excitation_degree_vector_mono(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
        do ispin = 1, 2  ! spin of the couple a-a^dagger (iorb,rorb)
         do jspin = 1, 2   ! spin of the couple a-a^dagger (aorb,vorb)
           do a = 1, n_act_orb      ! First active 
            aorb = list_act(a)
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
            perturb_dets_phase(a,jspin,ispin) = 0.0
            perturb_dets_hij(a,jspin,ispin) = 0.d0
            do istate = 1, N_states
             coef_perturb_from_idet(a,jspin,ispin,istate) = 0.d0
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
            delta_e(a,jspin,istate) = one_anhil(a,jspin,istate)                                                          &
                                    - fock_virt_total_spin_trace(rorb,istate)               & 
                                    - fock_virt_total_spin_trace(vorb,istate)               & 
                                    + fock_core_inactive_total_spin_trace(iorb,istate)        
           enddo
           if(ispin == jspin)then
            perturb_dets_hij(a,jspin,ispin) = phase * (active_int(a,1) - active_int(a,2) )
           else 
            perturb_dets_hij(a,jspin,ispin) = phase * active_int(a,1) 
           endif
!!!!!!!!!!!!!!!!!!!!!1 Computation of the coefficient at first order coming from idet 
!!!!!!!!!!!!!!!!!!!!!  for the excitation (i,j)(ispin,jspin)  ---> (r,a)(ispin,jspin)
           do istate = 1, N_states
            coef_perturb_from_idet(a,jspin,ispin,istate) = perturb_dets_hij(a,jspin,ispin) / delta_e(a,jspin,istate)
           enddo

         enddo
        enddo
       enddo
       
!!!!!!!!!!!!!!!!!!!!!!!!!!! determination of the connections between I and the other J determinants mono excited in the CAS
!!!!!!!!!!!!!!!!!!!!!!!!!!!! the determinants I and J must be connected by the following operator 
!!!!!!!!!!!!!!!!!!!!!!!!!!!! <Jdet | a^{\dagger}_b a_{a}  | Idet> 
       do jdet = 1, idx(0)
         if(idx(jdet).ne.idet)then
          call get_mono_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
          if (exc(0,1,1) == 1) then
            ! Mono alpha
            index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,1))   !!!  a_a
            index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,2,1))   !!!  a^{\dagger}_{b}
            index_orb_act_mono(idx(jdet),3) = 1
          else
            ! Mono beta
            index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,1,2))   !!!  a_a
            index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,2,2))   !!!  a^{\dagger}_{b}
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
         aorb  = index_orb_act_mono(idx(jdet),1)   !  a_{aorb}
         borb  = index_orb_act_mono(idx(jdet),2)   !  a^{\dagger}_{borb}
         kspin = index_orb_act_mono(idx(jdet),3)   !  spin of the excitation 
         ! the determinants Idet and Jdet interact throw the following operator 
         !      | Jdet > = a^{\dagger}_{borb,kspin} a_{aorb, kspin} | Idet >

         do ispin = 1, 2  ! you loop on all possible spin for the excitation 
                          !  a^{\dagger}_r a_{i} (ispin)
          if(ispin == kspin .and. vorb.le.rorb)cycle ! condition not to double count 

          ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{aorb,kspin} a_{iorb,ispin} | Idet > 
          do inint = 1, N_int
           det_tmp(inint,1)  = perturb_dets(inint,1,aorb,kspin,ispin) 
           det_tmp(inint,2)  = perturb_dets(inint,2,aorb,kspin,ispin) 
          enddo
          double precision :: hja
          ! you determine the interaction between the excited determinant and the other parent | Jdet >
          ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{borb,kspin} a_{iorb,ispin} | Jdet >
          ! hja = < det_tmp | H | Jdet >

          call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp,exc,phase,N_int)
          if(kspin == ispin)then
           hja = phase * (active_int(borb,1) - active_int(borb,2) )
          else
           hja = phase * active_int(borb,1)  
          endif

          do istate = 1, N_states
           matrix_1h2p(idx(jdet),idet,istate) += hja * coef_perturb_from_idet(aorb,kspin,ispin,istate)
          enddo
         enddo ! ispin 

        else
        ! diagonal part of the dressing : interaction of | Idet > with all the perturbers generated by the excitations 
        ! 
        ! | det_tmp > = a^{\dagger}_{rorb,ispin} a^{\dagger}_{vorb,kspin} a_{aorb,kspin} a_{iorb,ispin} | Idet > 
         do ispin = 1, 2
          do kspin = 1, 2
           do a = 1, n_act_orb      ! First active 
            aorb = list_act(a)
            if(ispin == kspin .and. vorb.le.rorb)cycle ! condition not to double count 
            do istate = 1, N_states
             matrix_1h2p(idet,idet,istate) += coef_perturb_from_idet(a,kspin,ispin,istate) * perturb_dets_hij(a,kspin,ispin) 
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


subroutine give_1h1p_contrib(matrix_1h1p)
  use bitmasks
 implicit none
 double precision , intent(inout) :: matrix_1h1p(N_det,N_det,*)
 integer :: i,j,r,a,b
 integer :: iorb, jorb, rorb, aorb, borb
 integer :: ispin,jspin
 integer :: idet,jdet
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 integer :: istate
 double precision :: hja,delta_e_inact_virt(N_states)
 integer :: kspin,degree_scalar
!matrix_1h1p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_ref(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_ref(inint,2,1))
 enddo
 do i = 1, n_inact_orb  ! First inactive 
  iorb = list_inact(i)
    do r = 1, n_virt_orb    ! First virtual
     rorb = list_virt(r) 
     do j = 1, N_states
      delta_e_inact_virt(j) = fock_core_inactive_total_spin_trace(iorb,j) & 
                            - fock_virt_total_spin_trace(rorb,j) 
     enddo
      do idet = 1, N_det
        call get_excitation_degree_vector_mono(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Case of the mono excitations
           do jdet = 1, idx(0)
            do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
               do inint = 1, N_int
                det_tmp(inint,1) = psi_ref(inint,1,idet)
                det_tmp(inint,2) = psi_ref(inint,2,idet)
               enddo
               ! Do the excitation  inactive -- > virtual
               double precision :: himono,delta_e(N_states),coef_mono(N_states)
               call clear_bit_to_integer(iorb,det_tmp(1,ispin),N_int)  ! hole in "iorb" of spin Ispin
               call set_bit_to_integer(rorb,det_tmp(1,ispin),N_int)    ! particle in "rorb" of spin Ispin
               call  i_H_j(psi_ref(1,1,idet),det_tmp,N_int,himono)
               
               do state_target = 1, N_states
!               delta_e(state_target) = one_anhil_one_creat_inact_virt(i,r,state_target) + delta_e_inact_virt(state_target)
                delta_e(state_target) = one_anhil_one_creat_inact_virt_bis(i,r,idet,state_target)
                coef_mono(state_target) = himono / delta_e(state_target)
               enddo
               if(idx(jdet).ne.idet)then
                call get_mono_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
                if (exc(0,1,1) == 1) then
                  ! Mono alpha
                  aorb  = (exc(1,2,1))   !!! a^{\dagger}_a 
                  borb  = (exc(1,1,1))   !!! a_{b}
                  jspin = 1
                else
                  ! Mono beta
                  aorb  = (exc(1,2,2))   !!!  a^{\dagger}_a
                  borb  = (exc(1,1,2))   !!!  a_{b}
                  jspin = 2
                endif
                
                call get_excitation_degree(psi_ref(1,1,idx(jdet)),det_tmp,degree_scalar,N_int)
                if(degree_scalar .ne. 2)then
                 print*, 'pb !!!'
                 print*, degree_scalar
                 call debug_det(psi_ref(1,1,idx(jdet)),N_int)
                 call debug_det(det_tmp,N_int)
                 stop
                endif
                call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp,exc,phase,N_int)
                if(ispin == jspin )then 
                 hij = -get_mo_bielec_integral(iorb,aorb,rorb,borb,mo_integrals_map) & 
                     +  get_mo_bielec_integral(iorb,aorb,borb,rorb,mo_integrals_map)
                else
                 hij =  get_mo_bielec_integral(iorb,borb,rorb,aorb,mo_integrals_map)   
                endif
                hij = hij * phase
                double precision :: hij_test
                integer  :: state_target
                call  i_H_j(psi_ref(1,1,idx(jdet)),det_tmp,N_int,hij_test)
                if(dabs(hij - hij_test).gt.1.d-10)then
                 print*, 'ahah pb !!'
                 print*, 'hij .ne. hij_test'
                 print*, hij,hij_test
                 call debug_det(psi_ref(1,1,idx(jdet)),N_int)
                 call debug_det(det_tmp,N_int)
                 print*, ispin, jspin
                 print*,iorb,borb,rorb,aorb 
                print*, phase
                call  i_H_j_verbose(psi_ref(1,1,idx(jdet)),det_tmp,N_int,hij_test)
                 stop
                endif
                do state_target = 1, N_states
                 matrix_1h1p(idx(jdet),idet,state_target) += hij* coef_mono(state_target)
                enddo
               else
                do state_target = 1, N_states
                 matrix_1h1p(idet,idet,state_target) += himono * coef_mono(state_target)
                enddo
               endif
            enddo
           enddo

       
        
      enddo
    enddo
 enddo
end

subroutine give_1h1p_sec_order_singles_contrib(matrix_1h1p)
  use bitmasks
 implicit none
 double precision , intent(inout) :: matrix_1h1p(N_det,N_det,*)
 integer :: i,j,r,a,b
 integer :: iorb, jorb, rorb, aorb, borb,s,sorb
 integer :: ispin,jspin
 integer :: idet,jdet
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2),det_tmp_bis(N_int,2)
 integer(bit_kind) :: det_pert(N_int,2,n_inact_orb,n_virt_orb,2)
 double precision :: coef_det_pert(n_inact_orb,n_virt_orb,2,N_states,2)
 double precision :: delta_e_det_pert(n_inact_orb,n_virt_orb,2,N_states)
 double precision :: hij_det_pert(n_inact_orb,n_virt_orb,2,N_states)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 integer :: istate
 double precision :: hja,delta_e_inact_virt(N_states)
 integer :: kspin,degree_scalar
!matrix_1h1p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_ref(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_ref(inint,2,1))
 enddo
 double precision :: himono,delta_e(N_states),coef_mono(N_states)
 integer  :: state_target
 do idet = 1, N_det
    call get_excitation_degree_vector_mono(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
    do i = 1, n_inact_orb  ! First inactive 
     iorb = list_inact(i)
      do r = 1, n_virt_orb    ! First virtual
       rorb = list_virt(r) 
       do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
        do state_target = 1, N_states
         coef_det_pert(i,r,ispin,state_target,1) = 0.d0
         coef_det_pert(i,r,ispin,state_target,2) = 0.d0
        enddo
        do j = 1, N_states
         delta_e_inact_virt(j) = fock_core_inactive_total_spin_trace(iorb,j) & 
                               - fock_virt_total_spin_trace(rorb,j) 
        enddo
        do inint = 1, N_int
         det_tmp(inint,1) = psi_ref(inint,1,idet)
         det_tmp(inint,2) = psi_ref(inint,2,idet)
        enddo
        ! Do the excitation  inactive -- > virtual
        call clear_bit_to_integer(iorb,det_tmp(1,ispin),N_int)  ! hole in "iorb" of spin Ispin
        call set_bit_to_integer(rorb,det_tmp(1,ispin),N_int)    ! particle in "rorb" of spin Ispin
        call  i_H_j(psi_ref(1,1,idet),det_tmp,N_int,himono)
        do inint = 1, N_int
         det_pert(inint,1,i,r,ispin) = det_tmp(inint,1)
         det_pert(inint,2,i,r,ispin) = det_tmp(inint,2)
        enddo
        do state_target = 1, N_states
         delta_e_det_pert(i,r,ispin,state_target) = one_anhil_one_creat_inact_virt(i,r,state_target) + delta_e_inact_virt(state_target)
         coef_det_pert(i,r,ispin,state_target,1) = himono / delta_e_det_pert(i,r,ispin,state_target)
        enddo
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Case of the mono excitations
       enddo ! ispin
      enddo ! rorb 
    enddo ! iorb 

    do i = 1, n_inact_orb  ! First inactive 
     iorb = list_inact(i)
      do r = 1, n_virt_orb    ! First virtual
       rorb = list_virt(r) 
       do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
        do inint = 1, N_int
         det_tmp(inint,1) = det_pert(inint,1,i,r,ispin)
         det_tmp(inint,2) = det_pert(inint,2,i,r,ispin)
        enddo
        do j = 1, n_inact_orb  ! First inactive 
         jorb = list_inact(j)
          do s = 1, n_virt_orb    ! First virtual
           sorb = list_virt(s) 
           do jspin = 1, 2  ! spin of the couple a-a^dagger (i,r)
            if(i==j.and.r==s.and.ispin==jspin)cycle
            do inint = 1, N_int
             det_tmp_bis(inint,1) = det_pert(inint,1,j,s,jspin)
             det_tmp_bis(inint,2) = det_pert(inint,2,j,s,jspin)
            enddo
            call  i_H_j(det_tmp_bis,det_tmp,N_int,himono)
            do state_target = 1, N_states
             coef_det_pert(i,r,ispin,state_target,2)  += & 
             coef_det_pert(j,s,jspin,state_target,1) * himono / delta_e_det_pert(i,r,ispin,state_target)
            enddo
           enddo
          enddo
         enddo
       enddo ! ispin
      enddo ! rorb 
    enddo ! iorb 
    do i = 1, n_inact_orb  ! First inactive 
     iorb = list_inact(i)
      do r = 1, n_virt_orb    ! First virtual
       rorb = list_virt(r) 
       do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
         do state_target = 1, N_states
          coef_det_pert(i,r,ispin,state_target,1)  += coef_det_pert(i,r,ispin,state_target,2)
         enddo
        
        do inint = 1, N_int
         det_tmp(inint,1) = det_pert(inint,1,i,r,ispin)
         det_tmp(inint,2) = det_pert(inint,2,i,r,ispin)
        enddo
        do jdet = 1, idx(0)
!      
         if(idx(jdet).ne.idet)then
          call get_mono_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
          if (exc(0,1,1) == 1) then
            ! Mono alpha
            aorb  = (exc(1,2,1))   !!! a^{\dagger}_a 
            borb  = (exc(1,1,1))   !!! a_{b}
            jspin = 1
          else
            aorb  = (exc(1,2,2))   !!!  a^{\dagger}_a
            borb  = (exc(1,1,2))   !!!  a_{b}
            jspin = 2
          endif
          
          call get_excitation_degree(psi_ref(1,1,idx(jdet)),det_tmp,degree_scalar,N_int)
          if(degree_scalar .ne. 2)then
           print*, 'pb !!!'
           print*, degree_scalar
           call debug_det(psi_ref(1,1,idx(jdet)),N_int)
           call debug_det(det_tmp,N_int)
           stop
          endif
          call get_double_excitation(psi_ref(1,1,idx(jdet)),det_tmp,exc,phase,N_int)
          double precision :: hij_test
          hij_test = 0.d0
          call  i_H_j(psi_ref(1,1,idx(jdet)),det_tmp,N_int,hij_test)
          do state_target = 1, N_states
           matrix_1h1p(idx(jdet),idet,state_target) += hij_test* coef_det_pert(i,r,ispin,state_target,2)
          enddo
         else
          hij_test = 0.d0
          call  i_H_j(psi_ref(1,1,idet),det_tmp,N_int,hij_test)
          do state_target = 1, N_states
           matrix_1h1p(idet,idet,state_target) += hij_test* coef_det_pert(i,r,ispin,state_target,2)
          enddo
         endif
      enddo
    enddo
   enddo
  enddo
      
 enddo ! idet
end


subroutine give_1p_sec_order_singles_contrib(matrix_1p)
  use bitmasks
 implicit none
 double precision , intent(inout) :: matrix_1p(N_det,N_det,*)
 integer :: i,j,r,a,b
 integer :: iorb, jorb, rorb, aorb, borb,s,sorb
 integer :: ispin,jspin
 integer :: idet,jdet
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2),det_tmp_bis(N_int,2)
 integer(bit_kind) :: det_pert(N_int,2,n_act_orb,n_virt_orb,2)
 double precision :: coef_det_pert(n_act_orb,n_virt_orb,2,N_states,2)
 double precision :: delta_e_det_pert(n_act_orb,n_virt_orb,2,N_states)
 double precision :: hij_det_pert(n_act_orb,n_virt_orb,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: hij,phase
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 integer :: istate
 double precision :: hja,delta_e_act_virt(N_states)
 integer :: kspin,degree_scalar
!matrix_1p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_ref(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_ref(inint,2,1))
 enddo
 double precision :: himono,delta_e(N_states),coef_mono(N_states)
 integer  :: state_target
 do idet = 1, N_det
    call get_excitation_degree_vector_mono(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
    do i = 1, n_act_orb  ! First active 
     iorb = list_act(i)
      do r = 1, n_virt_orb    ! First virtual
       rorb = list_virt(r) 
       do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
        do state_target = 1, N_states
         coef_det_pert(i,r,ispin,state_target,1) = 0.d0
         coef_det_pert(i,r,ispin,state_target,2) = 0.d0
        enddo
        do j = 1, N_states
         delta_e_act_virt(j) = - fock_virt_total_spin_trace(rorb,j) 
        enddo
        do inint = 1, N_int
         det_tmp(inint,1) = psi_ref(inint,1,idet)
         det_tmp(inint,2) = psi_ref(inint,2,idet)
        enddo
        ! Do the excitation  active -- > virtual
        call do_mono_excitation(det_tmp,iorb,rorb,ispin,i_ok)
        integer :: i_ok
        if(i_ok .ne.1)then
         do state_target = 1, N_states
          coef_det_pert(i,r,ispin,state_target,1) = -1.d+10
          coef_det_pert(i,r,ispin,state_target,2) = -1.d+10
          hij_det_pert(i,r,ispin) = 0.d0
         enddo
         do inint = 1, N_int
          det_pert(inint,1,i,r,ispin) = 0_bit_kind
          det_pert(inint,2,i,r,ispin) = 0_bit_kind
         enddo
         cycle
        endif
        call  i_H_j(psi_ref(1,1,idet),det_tmp,N_int,himono)
        do inint = 1, N_int
         det_pert(inint,1,i,r,ispin) = det_tmp(inint,1)
         det_pert(inint,2,i,r,ispin) = det_tmp(inint,2)
        enddo
        do state_target = 1, N_states
         delta_e_det_pert(i,r,ispin,state_target) = one_creat_virt(i,r,state_target) + delta_e_act_virt(state_target)
         coef_det_pert(i,r,ispin,state_target,1) = himono / delta_e_det_pert(i,r,ispin,state_target)
         hij_det_pert(i,r,ispin) = himono
        enddo
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Case of the mono excitations
       enddo ! ispin
      enddo ! rorb 
    enddo ! iorb 

!   do i = 1, n_act_orb  ! First active 
!    do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
!    if(coef_det_pert(i,1,ispin,1,1) == -1.d+10)cycle
!    iorb = list_act(i)
!     do r = 1, n_virt_orb    ! First virtual
!      rorb = list_virt(r) 
!       do inint = 1, N_int
!        det_tmp(inint,1) = det_pert(inint,1,i,r,ispin)
!        det_tmp(inint,2) = det_pert(inint,2,i,r,ispin)
!       enddo
!       do j = 1, n_act_orb  ! First active 
!        do jspin = 1, 2  ! spin of the couple a-a^dagger (i,r)
!        if(coef_det_pert(j,1,jspin,1,1) == -1.d+10)cycle
!        jorb = list_act(j)
!         do s = 1, n_virt_orb    ! First virtual
!          sorb = list_virt(s) 
!           if(i==j.and.r==s.and.ispin==jspin)cycle
!           do inint = 1, N_int
!            det_tmp_bis(inint,1) = det_pert(inint,1,j,s,jspin)
!            det_tmp_bis(inint,2) = det_pert(inint,2,j,s,jspin)
!           enddo
!           call  i_H_j(det_tmp_bis,det_tmp,N_int,himono)
!           do state_target = 1, N_states
!            coef_det_pert(i,r,ispin,state_target,2)  += & 
!            coef_det_pert(j,s,jspin,state_target,1) * himono / delta_e_det_pert(i,r,ispin,state_target)
!           enddo
!          enddo
!         enddo
!        enddo
!      enddo ! ispin
!     enddo ! rorb 
!   enddo ! iorb 

    do i = 1, n_act_orb  ! First active 
     do ispin = 1, 2  ! spin of the couple a-a^dagger (i,r)
     if(coef_det_pert(i,1,ispin,1,1) == -1.d+10)cycle
     iorb = list_act(i)
      do r = 1, n_virt_orb    ! First virtual
       rorb = list_virt(r) 
!        do state_target = 1, N_states
!         coef_det_pert(i,r,ispin,state_target,1)  += coef_det_pert(i,r,ispin,state_target,2)
!        enddo
        do inint = 1, N_int
         det_tmp(inint,1) = det_pert(inint,1,i,r,ispin)
         det_tmp(inint,2) = det_pert(inint,2,i,r,ispin)
        enddo
        do jdet = 1,N_det
         double precision :: coef_array(N_states),hij_test
         call  i_H_j(det_tmp,psi_ref(1,1,jdet),N_int,himono)
         call get_delta_e_dyall(psi_ref(1,1,jdet),det_tmp,delta_e)
         do state_target = 1, N_states
!         matrix_1p(idet,jdet,state_target) += himono * coef_det_pert(i,r,ispin,state_target,1)
          matrix_1p(idet,jdet,state_target) += himono * hij_det_pert(i,r,ispin) / delta_e(state_target)
         enddo
        enddo
    enddo
   enddo
  enddo
      
 enddo ! idet
end



subroutine give_1h1p_only_doubles_spin_cross(matrix_1h1p)
  use bitmasks
 implicit none
 double precision , intent(inout) :: matrix_1h1p(N_det,N_det,*)
 integer :: i,j,r,a,b
 integer :: iorb, jorb, rorb, aorb, borb
 integer :: ispin,jspin
 integer :: idet,jdet
 integer :: inint
 integer :: elec_num_tab_local(2),acu_elec
 integer(bit_kind) :: det_tmp(N_int,2)
 integer :: exc(0:2,2,2)
 integer :: accu_elec
 double precision :: get_mo_bielec_integral
 double precision :: active_int(n_act_orb,2)
 double precision :: hij,phase
 integer           :: degree(N_det)
 integer           :: idx(0:N_det)
 integer :: istate
 double precision :: hja,delta_e_inact_virt(N_states)
 integer(bit_kind) :: pert_det(N_int,2,n_act_orb,n_act_orb,2)
 double precision  :: pert_det_coef(n_act_orb,n_act_orb,2,N_states)
 integer :: kspin,degree_scalar
 integer :: other_spin(2)
 other_spin(1) = 2
 other_spin(2) = 1
 double precision :: hidouble,delta_e(N_states)
!matrix_1h1p = 0.d0
 
 elec_num_tab_local = 0
 do inint = 1, N_int
  elec_num_tab_local(1) += popcnt(psi_ref(inint,1,1))
  elec_num_tab_local(2) += popcnt(psi_ref(inint,2,1))
 enddo
 do i = 1, n_inact_orb  ! First inactive 
  iorb = list_inact(i)
    do r = 1, n_virt_orb    ! First virtual
     rorb = list_virt(r) 
     do j = 1, N_states
      delta_e_inact_virt(j) = fock_core_inactive_total_spin_trace(iorb,j) & 
                            - fock_virt_total_spin_trace(rorb,j) 
     enddo
      do idet = 1, N_det
        call get_excitation_degree_vector_double_alpha_beta(psi_ref,psi_ref(1,1,idet),degree,N_int,N_det,idx)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Precomputation of matrix elements 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Case of the mono excitations
           do ispin = 1, 2 
            jspin = other_spin(ispin)
            do a = 1, n_act_orb
             aorb = list_act(a)
             do b = 1, n_act_orb
              borb = list_act(b)
               do inint = 1, N_int
                det_tmp(inint,1) = psi_ref(inint,1,idet)
                det_tmp(inint,2) = psi_ref(inint,2,idet)
               enddo
               ! Do the excitation  (i-->a)(ispin)  + (b-->r)(other_spin(ispin))
               integer :: i_ok,corb,dorb
               call do_mono_excitation(det_tmp,iorb,aorb,ispin,i_ok)
               if(i_ok .ne. 1)then
                do state_target = 1, N_states
                 pert_det_coef(a,b,ispin,state_target) = -100000.d0
                enddo
                do inint = 1, N_int
                 pert_det(inint,1,a,b,ispin) = 0_bit_kind
                 pert_det(inint,2,a,b,ispin) = 0_bit_kind
                enddo
                cycle
               endif
               call do_mono_excitation(det_tmp,borb,rorb,jspin,i_ok)
               if(i_ok .ne. 1)then
                do state_target = 1, N_states
                 pert_det_coef(a,b,ispin,state_target) = -100000.d0
                enddo
                do inint = 1, N_int
                 pert_det(inint,1,a,b,ispin) = 0_bit_kind
                 pert_det(inint,2,a,b,ispin) = 0_bit_kind
                enddo
                cycle
               endif
               do inint = 1, N_int
                pert_det(inint,1,a,b,ispin) = det_tmp(inint,1)
                pert_det(inint,2,a,b,ispin) = det_tmp(inint,2)
               enddo

               call  i_H_j(psi_ref(1,1,idet),det_tmp,N_int,hidouble)
               do state_target = 1, N_states
                delta_e(state_target) = one_anhil_one_creat(a,b,ispin,jspin,state_target) +  delta_e_inact_virt(state_target)
                pert_det_coef(a,b,ispin,state_target) = hidouble / delta_e(state_target) 
                matrix_1h1p(idet,idet,state_target) += hidouble * pert_det_coef(a,b,ispin,state_target)
               enddo
             enddo
            enddo
           enddo
           do jdet = 1, idx(0)
               if(idx(jdet).ne.idet)then
                call get_double_excitation(psi_ref(1,1,idet),psi_ref(1,1,idx(jdet)),exc,phase,N_int)
                integer :: c,d,state_target
                integer(bit_kind) :: det_tmp_bis(N_int,2)
                ! excitation from I --> J 
                ! (a->c) (alpha) + (b->d) (beta)
                aorb =  exc(1,1,1) 
                corb =  exc(1,2,1)
                c = list_act_reverse(corb)
                borb =  exc(1,1,2)
                dorb =  exc(1,2,2)
                d = list_act_reverse(dorb)
                ispin = 1
                jspin = 2
                do inint = 1, N_int
                 det_tmp(inint,1) = pert_det(inint,1,c,d,1)
                 det_tmp(inint,2) = pert_det(inint,2,c,d,1)
                 det_tmp_bis(inint,1) = pert_det(inint,1,c,d,2)
                 det_tmp_bis(inint,2) = pert_det(inint,2,c,d,2)
                enddo
                double precision :: hjdouble_1,hjdouble_2
                call  i_H_j(psi_ref(1,1,idx(jdet)),det_tmp,N_int,hjdouble_1)
                call  i_H_j(psi_ref(1,1,idx(jdet)),det_tmp_bis,N_int,hjdouble_2)
                do state_target = 1, N_states
                 matrix_1h1p(idx(jdet),idet,state_target) += (pert_det_coef(c,d,1,state_target) * hjdouble_1 + pert_det_coef(c,d,2,state_target) * hjdouble_2 ) 
                enddo
               endif
           enddo

       
        
      enddo
    enddo
 enddo





end


