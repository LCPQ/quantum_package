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
          call get_mono_excitation(psi_det(1,1,idet),psi_det(1,1,idx(jdet)),exc,phase,N_int)
          if (exc(0,1,1) == 1) then
            ! Mono alpha
            index_orb_act_mono(idx(jdet),2) = list_act_reverse(exc(1,1,1))   !!!  a^{\dagger}_a
            index_orb_act_mono(idx(jdet),1) = list_act_reverse(exc(1,2,1))   !!!  a_{b}
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
