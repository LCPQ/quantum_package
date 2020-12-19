use omp_lib
use bitmasks

BEGIN_PROVIDER [ integer(omp_lock_kind), psi_ref_lock, (psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Locks on ref determinants to fill delta_ij
 END_DOC
 integer :: i
 do i=1,psi_det_size
   call omp_init_lock( psi_ref_lock(i) )
 enddo

END_PROVIDER


subroutine mrcc_dress(delta_ij_, Nstates, Ndet_non_ref, Ndet_ref,i_generator,n_selected,det_buffer,Nint,iproc,key_mask)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in)            :: Nstates, Ndet_ref, Ndet_non_ref
  double precision, intent(inout) :: delta_ij_(Nstates,Ndet_non_ref,Ndet_ref)

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,l,m
  integer                        :: degree_alpha(psi_det_size)
  integer                        :: idx_alpha(0:psi_det_size)
  logical                        :: good, fullMatch

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq, c_ref ,degree

  double precision               :: hIk, hla, hIl, dIk(Nstates), dka(Nstates), dIa(Nstates)
  double precision, allocatable  :: dIa_hla(:,:)
  double precision               :: haj, phase, phase2
  double precision               :: f(Nstates), ci_inv(Nstates)
  integer                        :: exc(0:2,2,2)
  integer                        :: h1,h2,p1,p2,s1,s2
  integer(bit_kind)              :: tmp_det(Nint,2)
  integer                        :: iint, ipos
  integer                        :: i_state, k_sd, l_sd, i_I, i_alpha
  
  integer(bit_kind),allocatable  :: miniList(:,:,:)
  integer(bit_kind),intent(in)   :: key_mask(Nint, 2)
  integer,allocatable            :: idx_miniList(:)
  integer                        :: N_miniList, ni, leng
  double precision, allocatable  :: hij_cache(:)
  
  integer(bit_kind), allocatable :: microlist(:,:,:), microlist_zero(:,:,:)
  integer, allocatable           :: idx_microlist(:), N_microlist(:), ptr_microlist(:), idx_microlist_zero(:)
  integer :: mobiles(2), smallerlist
  logical, external :: is_generable
  
  leng = max(N_det_generators, N_det_non_ref)
  allocate(miniList(Nint, 2, leng), idx_minilist(leng), hij_cache(N_det_non_ref))
  
  !create_minilist_find_previous(key_mask, fullList, miniList, N_fullList, N_miniList, fullMatch, Nint)
  call create_minilist_find_previous(key_mask, psi_det_generators, miniList, i_generator-1, N_miniList, fullMatch, Nint)
  
  if(fullMatch) then
    return
  end if
  
  allocate(ptr_microlist(0:mo_tot_num*2+1),  &
      N_microlist(0:mo_tot_num*2) )
  allocate(   microlist(Nint,2,N_minilist*4),               &
    idx_microlist(N_minilist*4))
  
  if(key_mask(1,1) /= 0_8) then
    call create_microlist(miniList, N_minilist, key_mask, microlist, idx_microlist, N_microlist, ptr_microlist, Nint)
    call find_triples_and_quadruples_micro(i_generator,n_selected,det_buffer,Nint,tq,N_tq,microlist,ptr_microlist,N_microlist,key_mask)
  else
    call find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_minilist)
  end if
  
  
  
  deallocate(microlist, idx_microlist)
  
  allocate (dIa_hla(Nstates,Ndet_non_ref))
  
  ! |I>
  
  ! |alpha>
  
  if(N_tq > 0) then

    call create_minilist(key_mask, psi_non_ref, miniList, idx_minilist, N_det_non_ref, N_minilist, Nint)
    if(N_minilist == 0) return
    
    
    if(key_mask(1,1) /= 0) then !!!!!!!!!!! PAS GENERAL !!!!!!!!!
      allocate(microlist_zero(Nint,2,N_minilist), idx_microlist_zero(N_minilist))
      
      allocate(   microlist(Nint,2,N_minilist*4),               &
        idx_microlist(N_minilist*4))
      call create_microlist(miniList, N_minilist, key_mask, microlist, idx_microlist, N_microlist, ptr_microlist, Nint)
      
      
      do i=0,mo_tot_num*2
        do k=ptr_microlist(i),ptr_microlist(i+1)-1
          idx_microlist(k) = idx_minilist(idx_microlist(k))
        end do
      end do
      
      do l=1,N_microlist(0)
        do k=1,Nint
          microlist_zero(k,1,l) = microlist(k,1,l)
          microlist_zero(k,2,l) = microlist(k,2,l)
        enddo
        idx_microlist_zero(l) = idx_microlist(l)
      enddo
    end if
  end if
  
  
      
  do i_alpha=1,N_tq
!     ok = .false.
!     do i=N_det_generators, 1, -1
!       if(is_generable(psi_det_generators(1,1,i), tq(1,1,i_alpha), Nint)) then
!         ok = .true.
!         exit
!       end if
!     end do
!     if(.not. ok) then
!         cycle
!     end if

    if(key_mask(1,1) /= 0) then
      call getMobiles(tq(1,1,i_alpha), key_mask, mobiles, Nint) 
      
      if(N_microlist(mobiles(1)) < N_microlist(mobiles(2))) then
        smallerlist = mobiles(1)
      else
        smallerlist = mobiles(2)
      end if
      
    
      do l=0,N_microlist(smallerlist)-1
        microlist_zero(:,:,ptr_microlist(1) + l) = microlist(:,:,ptr_microlist(smallerlist) + l)
        idx_microlist_zero(ptr_microlist(1) + l) = idx_microlist(ptr_microlist(smallerlist) + l)
      end do
      
      call get_excitation_degree_vector(microlist_zero,tq(1,1,i_alpha),degree_alpha,Nint,N_microlist(smallerlist)+N_microlist(0),idx_alpha)
      do j=1,idx_alpha(0)
        idx_alpha(j) = idx_microlist_zero(idx_alpha(j))
      end do
    else
      call get_excitation_degree_vector(miniList,tq(1,1,i_alpha),degree_alpha,Nint,N_minilist,idx_alpha)
      do j=1,idx_alpha(0)
        idx_alpha(j) = idx_miniList(idx_alpha(j))
      end do
    end if
    
    
    do l_sd=1,idx_alpha(0)
      k_sd = idx_alpha(l_sd)
      call i_h_j(tq(1,1,i_alpha),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hij_cache(k_sd))
    enddo

    ! |I>
    do i_I=1,N_det_ref
      ! Find triples and quadruple grand parents
      call get_excitation_degree(tq(1,1,i_alpha),psi_ref(1,1,i_I),degree,Nint)
      if (degree > 4) then
        cycle
      endif
      
      do i_state=1,Nstates
        dIa(i_state) = 0.d0
      enddo
      
      ! <I|  <>  |alpha>
      do k_sd=1,idx_alpha(0)
        
        ! Loop if lambda == 0
        logical                        :: loop
        loop = .True.
        do i_state=1,Nstates
          if (lambda_mrcc(i_state,idx_alpha(k_sd)) /= 0.d0) then
            loop = .False.
            exit
          endif
        enddo
        if (loop) then
          cycle
        endif
        
        call get_excitation_degree(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(k_sd)),degree,Nint)
        if (degree > 2) then
          cycle
        endif
        
        ! <I| /k\ |alpha>
        ! <I|H|k>
        hIk = hij_mrcc(idx_alpha(k_sd),i_I)
        !         call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(k_sd)),Nint,hIk)
        do i_state=1,Nstates
          dIk(i_state) = hIk * lambda_mrcc(i_state,idx_alpha(k_sd))
        enddo
        ! |l> = Exc(k -> alpha) |I>
        call get_excitation(psi_non_ref(1,1,idx_alpha(k_sd)),tq(1,1,i_alpha),exc,degree,phase,Nint)
        call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
        do k=1,N_int
          tmp_det(k,1) = psi_ref(k,1,i_I)
          tmp_det(k,2) = psi_ref(k,2,i_I)
        enddo
        
        logical :: ok
        call apply_excitation(psi_ref(1,1,i_I), exc, tmp_det, ok, Nint)
        if(.not. ok) cycle
        
        ! <I| \l/ |alpha>
        do i_state=1,Nstates
          dka(i_state) = 0.d0
        enddo
        do l_sd=k_sd+1,idx_alpha(0)

          call get_excitation_degree(tmp_det,psi_non_ref(1,1,idx_alpha(l_sd)),degree,Nint)
          if (degree == 0) then
            
            loop = .True.
            do i_state=1,Nstates
              if (lambda_mrcc(i_state,idx_alpha(l_sd)) /= 0.d0) then
                loop = .False.
                exit
              endif
            enddo
            if (.not.loop) then
              call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),exc,degree,phase2,Nint)
              hIl = hij_mrcc(idx_alpha(l_sd),i_I)
!                             call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hIl)
              do i_state=1,Nstates
                dka(i_state) = hIl * lambda_mrcc(i_state,idx_alpha(l_sd)) * phase * phase2
              enddo
            endif

            exit
          endif
        enddo
        do i_state=1,Nstates
          dIa(i_state) = dIa(i_state) + dIk(i_state) * dka(i_state)
        enddo
      enddo
      
      do i_state=1,Nstates
        ci_inv(i_state) = psi_ref_coef_inv(i_I,i_state)
      enddo
      do l_sd=1,idx_alpha(0)
        k_sd = idx_alpha(l_sd)
        hla = hij_cache(k_sd)
!        call i_h_j(tq(1,1,i_alpha),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hla)
        do i_state=1,Nstates
          dIa_hla(i_state,k_sd) = dIa(i_state) * hla
        enddo
      enddo
      call omp_set_lock( psi_ref_lock(i_I) )

      
      do i_state=1,Nstates
        if(dabs(psi_ref_coef(i_I,i_state)).ge.5.d-5)then
          do l_sd=1,idx_alpha(0)
            k_sd = idx_alpha(l_sd)
            delta_ij_(i_state,k_sd,i_I) = delta_ij_(i_state,k_sd,i_I) + dIa_hla(i_state,k_sd)
          enddo
        else
          do l_sd=1,idx_alpha(0)
            k_sd = idx_alpha(l_sd)
            delta_ij_(i_state,k_sd,i_I) = delta_ij_(i_state,k_sd,i_I) + 0.5d0 * dIa_hla(i_state,k_sd)
          enddo
        endif
      enddo
      call omp_unset_lock( psi_ref_lock(i_I) )
    enddo
  enddo
  deallocate (dIa_hla,hij_cache)
  deallocate(miniList, idx_miniList)
end


subroutine find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_miniList)

 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,m
  logical                        :: is_in_wavefunction
  integer                        :: degree(psi_det_size)
  integer                        :: idx(0:psi_det_size)
  logical                        :: good

  integer(bit_kind), intent(out) :: tq(Nint,2,n_selected)
  integer, intent(out)           :: N_tq
  
  
  integer                        :: nt,ni
  logical, external              :: is_connected_to
  
  
  integer(bit_kind),intent(in)  :: miniList(Nint,2,N_det_generators)
  integer,intent(in)            :: N_miniList

  
  
  N_tq = 0
  
  
  
  i_loop : do i=1,N_selected
    if(is_connected_to(det_buffer(1,1,i), miniList, Nint, N_miniList)) then
      cycle
    end if

    ! Select determinants that are triple or quadruple excitations
    ! from the ref
    good = .True.
    call get_excitation_degree_vector(psi_ref,det_buffer(1,1,i),degree,Nint,N_det_ref,idx) 
    !good=(idx(0) == 0) tant que degree > 2 pas retourné par get_excitation_degree_vector
    do k=1,idx(0)
      if (degree(k) < 3) then
        good = .False.
        exit
      endif
    enddo
    if (good) then
      if (.not. is_in_wavefunction(det_buffer(1,1,i),Nint)) then
        N_tq += 1
        do k=1,N_int
          tq(k,1,N_tq) = det_buffer(k,1,i)
          tq(k,2,N_tq) = det_buffer(k,2,i)
        enddo
      endif
    endif
  enddo i_loop
end


subroutine find_triples_and_quadruples_micro(i_generator,n_selected,det_buffer,Nint,tq,N_tq,microlist,ptr_microlist,N_microlist,key_mask)

 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,m
  logical                        :: is_in_wavefunction
  integer                        :: degree(psi_det_size)
  integer                        :: idx(0:psi_det_size)
  logical                        :: good

  integer(bit_kind), intent(out) :: tq(Nint,2,n_selected)
  integer, intent(out)           :: N_tq
  
  
  integer                        :: nt,ni
  logical, external              :: is_connected_to
  
  
  integer(bit_kind),intent(in)  :: microlist(Nint,2,*)
  integer,intent(in)  :: ptr_microlist(0:*)
  integer,intent(in)            :: N_microlist(0:*)
  integer(bit_kind),intent(in)   :: key_mask(Nint, 2)
  
  integer :: mobiles(2), smallerlist
  
  N_tq = 0
  
  
  
  i_loop : do i=1,N_selected
    call getMobiles(det_buffer(1,1,i), key_mask, mobiles, Nint) 
    if(N_microlist(mobiles(1)) < N_microlist(mobiles(2))) then
      smallerlist = mobiles(1)
    else
      smallerlist = mobiles(2)
    end if
    
    if(N_microlist(smallerlist) > 0) then
      if(is_connected_to(det_buffer(1,1,i), microlist(1,1,ptr_microlist(smallerlist)), Nint, N_microlist(smallerlist))) then
        cycle
      end if
    end if
    
    if(N_microlist(0) > 0) then
      if(is_connected_to(det_buffer(1,1,i), microlist, Nint, N_microlist(0))) then
        cycle
      end if
    end if

    ! Select determinants that are triple or quadruple excitations
    ! from the ref
    good = .True.
    call get_excitation_degree_vector(psi_ref,det_buffer(1,1,i),degree,Nint,N_det_ref,idx) 
    !good=(idx(0) == 0) tant que degree > 2 pas retourné par get_excitation_degree_vector
    do k=1,idx(0)
      if (degree(k) < 3) then
        good = .False.
        exit
      endif
    enddo
    if (good) then
      if (.not. is_in_wavefunction(det_buffer(1,1,i),Nint)) then
        N_tq += 1
        do k=1,N_int
          tq(k,1,N_tq) = det_buffer(k,1,i)
          tq(k,2,N_tq) = det_buffer(k,2,i)
        enddo
      endif
    endif
  enddo i_loop
end






