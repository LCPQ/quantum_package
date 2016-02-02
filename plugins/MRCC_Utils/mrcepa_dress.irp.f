use omp_lib
use bitmasks

subroutine mrcepa_dress(delta_ij_, delta_ii_, Ndet_ref, Ndet_non_ref,i_generator,n_selected,det_buffer,Nint,iproc,key_mask)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in)            :: Ndet_ref, Ndet_non_ref
  double precision, intent(inout) :: delta_ij_(Ndet_ref,Ndet_non_ref,*)
  double precision, intent(inout) :: delta_ii_(Ndet_ref,*)

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,l
  integer                        :: degree_alpha(psi_det_size)
  integer                        :: idx_alpha(0:psi_det_size)
  logical                        :: good, fullMatch

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq, c_ref ,degree

  double precision               :: hIk, hla, hIl, dIk(N_states), dka(N_states), dIa(N_states)
  double precision, allocatable  :: dIa_hia(:,:)
  double precision               :: haj, phase, phase2
  double precision               :: f(N_states), ci_inv(N_states)
  integer                        :: exc(0:2,2,2)
  integer                        :: h1,h2,p1,p2,s1,s2
  integer(bit_kind)              :: tmp_det(Nint,2)
  integer(bit_kind)              :: tmp_det_0(Nint,2)
  integer                        :: iint, ipos
  integer                        :: i_state, i_sd, k_sd, l_sd, i_I, i_alpha
  
  integer(bit_kind),allocatable  :: miniList(:,:,:)
  integer(bit_kind),intent(in)   :: key_mask(Nint, 2)
  integer,allocatable            :: idx_miniList(:)
  integer                        :: N_miniList, ni, leng
  integer(bit_kind)              :: isum

  double precision :: hia
  integer, allocatable           :: index_sorted(:)
  
  
  leng = max(N_det_generators, N_det_non_ref)
  allocate(miniList(Nint, 2, leng), idx_miniList(leng), index_sorted(N_det))
  
  !create_minilist_find_previous(key_mask, fullList, miniList, N_fullList, N_miniList, fullMatch, Nint)
  call create_minilist_find_previous(key_mask, psi_det_generators, miniList, i_generator-1, N_miniList, fullMatch, Nint)
  
  if(fullMatch) then
    return
  end if
  
  
  call find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_minilist)

  allocate (dIa_hia(N_states,Ndet_non_ref))

  ! |I>

  ! |alpha>
  
   if(N_tq > 0) then
     call create_minilist(key_mask, psi_non_ref, miniList, idx_miniList, N_det_non_ref, N_minilist, Nint)
   end if
  
  
  do i_alpha=1,N_tq
    !    call get_excitation_degree_vector(psi_non_ref,tq(1,1,i_alpha),degree_alpha,Nint,N_det_non_ref,idx_alpha)
    call get_excitation_degree_vector(miniList,tq(1,1,i_alpha),degree_alpha,Nint,N_minilist,idx_alpha)
    
    integer, external              :: get_index_in_psi_det_sorted_bit
    index_sorted = huge(-1)
    do j=1,idx_alpha(0)
      idx_alpha(j) = idx_miniList(idx_alpha(j))
      index_sorted( get_index_in_psi_det_sorted_bit( psi_non_ref(1,1,idx_alpha(j)), N_int ) ) = idx_alpha(j)
    end do

    ! |I>
    do i_I=1,N_det_ref
      ! Find triples and quadruple grand parents
      call get_excitation_degree(tq(1,1,i_alpha),psi_ref(1,1,i_I),degree,Nint)
      if (degree > 4) then
        cycle
      endif
      
      do i_state=1,N_states
        dIa(i_state) = 0.d0
      enddo
      
      !TODO: MR
      do i_sd=1,idx_alpha(0)
        call get_excitation_degree(psi_non_ref(1,1,idx_alpha(i_sd)),tq(1,1,i_alpha),degree,Nint)
        if (degree > 2) then
          cycle
        endif
        call get_excitation(psi_non_ref(1,1,idx_alpha(i_sd)),tq(1,1,i_alpha),exc,degree,phase,Nint)
        call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
        tmp_det_0 = 0_bit_kind
        ! Hole (see list_to_bitstring)
        iint = ishft(h1-1,-bit_kind_shift) + 1
        ipos = h1-ishft((iint-1),bit_kind_shift)-1
        tmp_det_0(iint,s1) = ibset(tmp_det_0(iint,s1),ipos)
        
        ! Particle
        iint = ishft(p1-1,-bit_kind_shift) + 1
        ipos = p1-ishft((iint-1),bit_kind_shift)-1
        tmp_det_0(iint,s1) = ibset(tmp_det_0(iint,s1),ipos)
        if (degree == 2) then
          ! Hole (see list_to_bitstring)
          iint = ishft(h2-1,-bit_kind_shift) + 1
          ipos = h2-ishft((iint-1),bit_kind_shift)-1
          tmp_det_0(iint,s2) = ibset(tmp_det_0(iint,s2),ipos)
          
          ! Particle
          iint = ishft(p2-1,-bit_kind_shift) + 1
          ipos = p2-ishft((iint-1),bit_kind_shift)-1
          tmp_det_0(iint,s2) = ibset(tmp_det_0(iint,s2),ipos)
        endif
        
        call i_h_j(tq(1,1,i_alpha),psi_non_ref(1,1,idx_alpha(i_sd)),Nint,hia)
        
        ! <I|  <>  |alpha>
        do k_sd=1,idx_alpha(0)
          call get_excitation_degree(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(k_sd)),degree,Nint)
          if (degree > 2) then
            cycle
          endif
          
          call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(k_sd)),exc,degree,phase,Nint)
          call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
          tmp_det = 0_bit_kind
          ! Hole (see list_to_bitstring)
          iint = ishft(h1-1,-bit_kind_shift) + 1
          ipos = h1-ishft((iint-1),bit_kind_shift)-1
          tmp_det(iint,s1) = ibset(tmp_det(iint,s1),ipos)
          
          ! Particle
          iint = ishft(p1-1,-bit_kind_shift) + 1
          ipos = p1-ishft((iint-1),bit_kind_shift)-1
          tmp_det(iint,s1) = ibset(tmp_det(iint,s1),ipos)
          if (degree == 2) then
            ! Hole (see list_to_bitstring)
            iint = ishft(h2-1,-bit_kind_shift) + 1
            ipos = h2-ishft((iint-1),bit_kind_shift)-1
            tmp_det(iint,s2) = ibset(tmp_det(iint,s2),ipos)
            
            ! Particle
            iint = ishft(p2-1,-bit_kind_shift) + 1
            ipos = p2-ishft((iint-1),bit_kind_shift)-1
            tmp_det(iint,s2) = ibset(tmp_det(iint,s2),ipos)
          endif

          isum = 0_bit_kind
          do iint = 1,N_int
            isum = isum + iand(tmp_det(iint,1), tmp_det_0(iint,1)) &
                        + iand(tmp_det(iint,2), tmp_det_0(iint,2))
          enddo

          if (isum /= 0_bit_kind) then
            cycle
          endif

          ! <I| /k\ |alpha>
          ! <I|H|k>
          call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(k_sd)),Nint,hIk)
          do i_state=1,N_states
            dIk(i_state) = hIk * lambda_mrcc(i_state,idx_alpha(k_sd))
          enddo
          ! |l> = Exc(k -> alpha) |I>
          call get_excitation(psi_non_ref(1,1,idx_alpha(k_sd)),tq(1,1,i_alpha),exc,degree,phase,Nint)
          call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
          do k=1,N_int
            tmp_det(k,1) = psi_ref(k,1,i_I)
            tmp_det(k,2) = psi_ref(k,2,i_I)
          enddo
          ! Hole (see list_to_bitstring)
          iint = ishft(h1-1,-bit_kind_shift) + 1
          ipos = h1-ishft((iint-1),bit_kind_shift)-1
          tmp_det(iint,s1) = ibclr(tmp_det(iint,s1),ipos)
          
          ! Particle
          iint = ishft(p1-1,-bit_kind_shift) + 1
          ipos = p1-ishft((iint-1),bit_kind_shift)-1
          tmp_det(iint,s1) = ibset(tmp_det(iint,s1),ipos)
          if (degree == 2) then
            ! Hole (see list_to_bitstring)
            iint = ishft(h2-1,-bit_kind_shift) + 1
            ipos = h2-ishft((iint-1),bit_kind_shift)-1
            tmp_det(iint,s2) = ibclr(tmp_det(iint,s2),ipos)
            
            ! Particle
            iint = ishft(p2-1,-bit_kind_shift) + 1
            ipos = p2-ishft((iint-1),bit_kind_shift)-1
            tmp_det(iint,s2) = ibset(tmp_det(iint,s2),ipos)
          endif
          
          ! <I| \l/ |alpha>
          do i_state=1,N_states
            dka(i_state) = 0.d0
          enddo


!          l_sd = index_sorted( get_index_in_psi_det_sorted_bit( tmp_det, N_int ) )
!          call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,l_sd),exc,degree,phase2,Nint)
!          call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,l_sd),Nint,hIl)
!          do i_state=1,N_states
!            dka(i_state) = hIl * lambda_mrcc(i_state,l_sd) * phase * phase2
!          enddo
    
          do l_sd=1,idx_alpha(0)
            call get_excitation_degree(tmp_det,psi_non_ref(1,1,idx_alpha(l_sd)),degree,Nint)
            if (degree == 0) then
              call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),exc,degree,phase2,Nint)
              call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hIl)
              do i_state=1,N_states
                dka(i_state) = hIl * lambda_mrcc(i_state,idx_alpha(l_sd)) * phase * phase2
              enddo
              exit
            endif
          enddo
          do i_state=1,N_states
            dIa(i_state) = dIa(i_state) + dIk(i_state) * dka(i_state)
          enddo
        enddo
        
        do i_state=1,N_states
          ci_inv(i_state) = 1.d0/psi_ref_coef(i_I,i_state)
        enddo

        k_sd = idx_alpha(i_sd)
        do i_state=1,N_states
          dIa_hia(i_state,k_sd) = dIa(i_state) * hia
        enddo
        
        call omp_set_lock( psi_ref_lock(i_I) )
        do i_state=1,N_states
          delta_ij_(i_I,k_sd,i_state) += dIa_hia(i_state,k_sd)

          if(dabs(psi_ref_coef(i_I,i_state)).ge.5.d-5)then
            delta_ii_(i_I,i_state) -= dIa_hia(i_state,k_sd) * ci_inv(i_state) * psi_non_ref_coef(k_sd,i_state)
          else
            delta_ii_(i_I,i_state)  = 0.d0
          endif
        enddo
        call omp_unset_lock( psi_ref_lock(i_I) )
      enddo
    enddo

  enddo
  deallocate (dIa_hia,index_sorted)
  deallocate(miniList, idx_miniList)
end








