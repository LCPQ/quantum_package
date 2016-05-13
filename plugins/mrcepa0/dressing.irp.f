use bitmasks


subroutine dec_exc(exc, h1, h2, p1, p2)
  implicit none
  integer :: exc(0:2,2,2), s1, s2, degree
  integer, intent(out) :: h1, h2, p1, p2
  
  degree = exc(0,1,1) + exc(0,1,2)
  
  h1 = 0
  h2 = 0
  p1 = 0
  p2 = 0
    
  if(degree == 0) return
  
  call decode_exc(exc, degree, h1, p1, h2, p2, s1, s2)
  
  h1 += mo_tot_num * (s1-1)
  p1 += mo_tot_num * (s1-1)
  
  if(degree == 2) then
    h2 += mo_tot_num * (s2-1)
    p2 += mo_tot_num * (s2-1)
    if(h1 > h2) then
      s1 = h1
      h1 = h2
      h2 = s1
    end if
    if(p1 > p2) then
      s1 = p1
      p1 = p2
      p2 = s1
    end if
  else
    h2 = h1
    p2 = p1
    p1 = 0
    h1 = 0
  end if
end subroutine



 BEGIN_PROVIDER [ integer, hh_exists, (4, N_det_ref * N_det_non_ref) ]
&BEGIN_PROVIDER [ integer, hh_shortcut, (0:N_det_ref * N_det_non_ref + 1) ]
&BEGIN_PROVIDER [ integer, pp_exists, (4, N_det_ref * N_det_non_ref) ]
  implicit none
  integer :: num(0:mo_tot_num*2, 0:mo_tot_num*2)
  integer :: exc(0:2, 2, 2), degree, n, on, s, h1, h2, p1, p2, l, i
  double precision :: phase
  
  hh_shortcut = 0
  hh_exists = 0
  pp_exists = 0
  num = 0
  
  do i=1, N_det_ref
    do l=1, N_det_non_ref
      call get_excitation(psi_ref(1,1,i), psi_non_ref(1,1,l), exc, degree, phase, N_int)
      if(degree == -1) cycle
      call dec_exc(exc, h1, h2, p1, p2)
      num(h1, h2) += 1
    end do
  end do
  
  n = 1
  do l=0,mo_tot_num*2
  do i=0,l
    on = num(i,l)
    if(on /= 0) then
      hh_shortcut(0) += 1
      hh_shortcut(hh_shortcut(0)) = n
      hh_exists(:, hh_shortcut(0)) = (/1, i, 1, l/)
    end if
    
    num(i,l) = n
    n += on
  end do
  end do
  
  hh_shortcut(hh_shortcut(0)+1) = n
  
  do i=1, N_det_ref
    do l=1, N_det_non_ref
      call get_excitation(psi_ref(1,1,i), psi_non_ref(1,1,l), exc, degree, phase, N_int)
      if(degree == -1) cycle
      call dec_exc(exc, h1, h2, p1, p2)
      pp_exists(:, num(h1, h2)) = (/1,p1,1,p2/)
      num(h1, h2) += 1
    end do
  end do
  
  do s=2,4,2
    do i=1,hh_shortcut(0)
      if(hh_exists(s, i) == 0) then
        hh_exists(s-1, i) = 0
      else if(hh_exists(s, i) > mo_tot_num) then
        hh_exists(s, i) -= mo_tot_num
        hh_exists(s-1, i) = 2
      end if
    end do
    
    do i=1,hh_shortcut(hh_shortcut(0)+1)-1
      if(pp_exists(s, i) == 0) then
        pp_exists(s-1, i) = 0
      else if(pp_exists(s, i) > mo_tot_num) then
        pp_exists(s, i) -= mo_tot_num
        pp_exists(s-1, i) = 2
      end if
    end do
  end do
  
END_PROVIDER




subroutine apply_hole(det, exc, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: exc(4)
  integer :: s1, s2, h1, h2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos
  
  ok = .false.
  s1 = exc(1)
  h1 = exc(2)
  s2 = exc(3)
  h2 = exc(4)
  res = det
  
  if(h1 /= 0) then
  ii = (h1-1)/bit_kind_size + 1 
  pos = mod(h1-1, 64)!iand(h1-1,bit_kind_size-1) ! mod 64
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s1) = ibclr(res(ii, s1), pos)
  end if
  
    ii = (h2-1)/bit_kind_size + 1 
    pos = mod(h2-1, 64)!iand(h2-1,bit_kind_size-1)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) == 0_8) return
    res(ii, s2) = ibclr(res(ii, s2), pos)

  
  ok = .true.
end subroutine


subroutine apply_particle(det, exc, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: exc(4)
  integer :: s1, s2, p1, p2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  s1 = exc(1)
  p1 = exc(2)
  s2 = exc(3)
  p2 = exc(4)
  res = det 
  
  if(p1 /= 0) then
  ii = (p1-1)/bit_kind_size + 1 
  pos = mod(p1-1, 64)!iand(p1-1,bit_kind_size-1)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s1) = ibset(res(ii, s1), pos)
  end if

    ii = (p2-1)/bit_kind_size + 1 
    pos = mod(p2-1, 64)!iand(p2-1,bit_kind_size-1)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) /= 0_8) return
    res(ii, s2) = ibset(res(ii, s2), pos)

  
  ok = .true.
end subroutine


 BEGIN_PROVIDER [ double precision, delta_ij_mrcc, (N_states,N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ii_mrcc, (N_states, N_det_ref) ]
  use bitmasks
  implicit none
  integer :: gen, h, p, i_state, n, t
  integer(bit_kind) :: mask(N_int, 2), omask(N_int, 2), buf(N_int, 2, N_det_non_ref)
  logical :: ok
  
  delta_ij_mrcc = 0d0
  delta_ii_mrcc = 0d0
  i_state = 1
  
  do gen=1, N_det_generators
    !print *, gen, "/", N_det_generators
    do h=1, hh_shortcut(0)
      call apply_hole(psi_det_generators(1,1,gen), hh_exists(1, h), mask, ok, N_int)
      if(.not. ok) cycle
      omask = 0
      if(hh_exists(1, h) /= 0) omask = mask
      !-459.6378590456251
      !-199.0659502581943
      n = 1
      ploop : do p=hh_shortcut(h), hh_shortcut(h+1)-1
        
        do t=hh_shortcut(h), p-1
          if(pp_exists(1, p) == pp_exists(1,t) .and. &
              pp_exists(2, p) == pp_exists(2,t) .and. &
              pp_exists(3, p) == pp_exists(3,t) .and. &
              pp_exists(4, p) == pp_exists(4,t)) cycle ploop
        end do
        call apply_particle(mask, pp_exists(1, p), buf(1,1,n), ok, N_int)
        !-459.6379081607463
        !-199.0659982685706
        if(ok) n = n + 1
      end do ploop
      n = n - 1
      if(n /= 0) call mrcc_part_dress(delta_ij_mrcc, delta_ii_mrcc,gen,n,buf,N_int,omask)
    end do
  end do
END_PROVIDER



subroutine mrcc_part_dress(delta_ij_, delta_ii_,i_generator,n_selected,det_buffer,Nint,key_mask)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint
  double precision, intent(inout) :: delta_ij_(N_states,N_det_non_ref,N_det_ref)
  double precision, intent(inout) :: delta_ii_(N_states,N_det_ref)

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,l,m
  integer                        :: degree_alpha(psi_det_size)
  integer                        :: idx_alpha(0:psi_det_size)
  logical                        :: good, fullMatch

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq, c_ref ,degree

  double precision               :: hIk, hla, hIl, dIk(N_states), dka(N_states), dIa(N_states)
  double precision, allocatable  :: dIa_hla(:,:)
  double precision               :: haj, phase, phase2
  double precision               :: f(N_states), ci_inv(N_states)
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
  
  if(key_mask(1,1) /= 0) then
    call create_microlist(miniList, N_minilist, key_mask, microlist, idx_microlist, N_microlist, ptr_microlist, Nint)
    call find_triples_and_quadruples_micro(i_generator,n_selected,det_buffer,Nint,tq,N_tq,microlist,ptr_microlist,N_microlist,key_mask)
  else
    call find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_minilist)
  end if
  
  
  
  deallocate(microlist, idx_microlist)
  
  allocate (dIa_hla(N_states,N_det_non_ref))
  
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
      
      do i_state=1,N_states
        dIa(i_state) = 0.d0
      enddo
      
      ! <I|  <>  |alpha>
      do k_sd=1,idx_alpha(0)
        
        ! Loop if lambda == 0
        logical                        :: loop
        loop = .True.
        do i_state=1,N_states
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
        
        logical :: ok
        call apply_excitation(psi_ref(1,1,i_I), exc, tmp_det, ok, Nint)
        if(.not. ok) cycle
        
        ! <I| \l/ |alpha>
        do i_state=1,N_states
          dka(i_state) = 0.d0
        enddo
        do l_sd=k_sd+1,idx_alpha(0)

          call get_excitation_degree(tmp_det,psi_non_ref(1,1,idx_alpha(l_sd)),degree,Nint)
          if (degree == 0) then
            
            loop = .True.
            do i_state=1,N_states
              if (lambda_mrcc(i_state,idx_alpha(l_sd)) /= 0.d0) then
                loop = .False.
                exit
              endif
            enddo
            if (.not.loop) then
              call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),exc,degree,phase2,Nint)
              hIl = hij_mrcc(idx_alpha(l_sd),i_I)
!                             call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hIl)
              do i_state=1,N_states
                dka(i_state) = hIl * lambda_mrcc(i_state,idx_alpha(l_sd)) * phase * phase2
              enddo
            endif

            exit
          endif
        enddo
        do i_state=1,N_states
          dIa(i_state) = dIa(i_state) + dIk(i_state) * dka(i_state)
        enddo
      enddo
      
      do i_state=1,N_states
        ci_inv(i_state) = psi_ref_coef_inv(i_I,i_state)
      enddo
      do l_sd=1,idx_alpha(0)
        k_sd = idx_alpha(l_sd)
        hla = hij_cache(k_sd)
!        call i_h_j(tq(1,1,i_alpha),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hla)
        do i_state=1,N_states
          dIa_hla(i_state,k_sd) = dIa(i_state) * hla
        enddo
      enddo
      call omp_set_lock( psi_ref_lock(i_I) )
      do i_state=1,N_states
        if(dabs(psi_ref_coef(i_I,i_state)).ge.5.d-5)then
          do l_sd=1,idx_alpha(0)
            k_sd = idx_alpha(l_sd)
              delta_ij_(i_state,k_sd,i_I) = delta_ij_(i_state,k_sd,i_I) + dIa_hla(i_state,k_sd)
              delta_ii_(i_state,i_I) = delta_ii_(i_state,i_I) - dIa_hla(i_state,k_sd) * ci_inv(i_state) * psi_non_ref_coef_transp(i_state,k_sd)
          enddo
        else
          delta_ii_(i_state,i_I)  = 0.d0
          do l_sd=1,idx_alpha(0)
            k_sd = idx_alpha(l_sd)
              delta_ij_(i_state,k_sd,i_I) = delta_ij_(i_state,k_sd,i_I) + dIa_hla(i_state,k_sd)
          enddo
        endif
      enddo
      call omp_unset_lock( psi_ref_lock(i_I) )
    enddo
  enddo
  !deallocate (dIa_hla,hij_cache)
  !deallocate(miniList, idx_miniList)
end




 BEGIN_PROVIDER [ double precision, delta_ij, (N_states,N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ii, (N_states, N_det_ref) ]
  use bitmasks
  implicit none
  integer :: i, j, i_state
  
  !mrmode : 1=mrcepa0, 2=mrsc2 add, 3=mrsc2 sub
  
  do i_state = 1, N_states
    if(mrmode == 3) then
      do i = 1, N_det_ref
        delta_ii(i_state,i)= delta_mrcepa0_ii(i,i_state) - delta_sub_ii(i,i_state)
        do j = 1, N_det_non_ref
          delta_ij(i_state,j,i) = delta_mrcepa0_ij(i,j,i_state) - delta_sub_ij(i,j,i_state)
        end do
      end do
    else if(mrmode == 2) then
      do i = 1, N_det_ref
        delta_ii(i_state,i)= delta_ii_mrcc(i_state,i)
        do j = 1, N_det_non_ref
          delta_ij(i_state,j,i) = delta_ij_mrcc(i_state,j,i)
        end do
      end do
!       do i = 1, N_det_ref
!         delta_ii(i_state,i)= delta_ii_old(i_state,i)
!         do j = 1, N_det_non_ref
!           delta_ij(i_state,j,i) = delta_ij_old(i_state,j,i)
!         end do
!       end do
    else if(mrmode == 1) then
      do i = 1, N_det_ref
        delta_ii(i_state,i)= delta_mrcepa0_ii(i,i_state)
        do j = 1, N_det_non_ref
          delta_ij(i_state,j,i) = delta_mrcepa0_ij(i,j,i_state)
        end do
      end do
    else
      stop "invalid mrmode"
    end if
  end do
END_PROVIDER


BEGIN_PROVIDER [ integer, HP, (2,N_det_non_ref) ]
  integer :: i
  do i=1,N_det_non_ref
    call getHP(psi_non_ref(1,1,i), HP(1,i), HP(2,i), N_int)
  end do 
END_PROVIDER

 BEGIN_PROVIDER [ integer, cepa0_shortcut, (0:N_det_non_ref+1) ]
&BEGIN_PROVIDER [ integer, det_cepa0_idx, (N_det_non_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), det_cepa0_active, (N_int,2,N_det_non_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), det_ref_active, (N_int,2,N_det_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), active_sorb, (N_int,2) ]
&BEGIN_PROVIDER [ integer(bit_kind), det_cepa0, (N_int,2,N_det_non_ref) ]
&BEGIN_PROVIDER [ integer, nlink, (N_det_ref) ]
&BEGIN_PROVIDER [ integer, linked, (N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ integer, blokMwen, (N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, searchance, (N_det_ref) ]
&BEGIN_PROVIDER [ integer, child_num, (N_det_non_ref,N_det_ref) ]

  use bitmasks
  implicit none
  
  integer(bit_kind) :: det_noactive(N_int, 2, N_det_non_ref), nonactive_sorb(N_int,2), det(N_int, 2)
  integer i, II, j, k, n, ni, idx(N_det_non_ref), shortcut(0:N_det_non_ref+1), blok, degree
  logical, external :: detEq
  
  active_sorb(:,:) = 0_8
  nonactive_sorb(:,:) = not(0_8)
  
  if(N_det_ref > 1) then
    do i=1, N_det_ref
    do k=1, N_int
      active_sorb(k,1) = ior(psi_ref(k,1,i), active_sorb(k,1))
      active_sorb(k,2) = ior(psi_ref(k,2,i), active_sorb(k,2))
      nonactive_sorb(k,1) = iand(psi_ref(k,1,i), nonactive_sorb(k,1))
      nonactive_sorb(k,2) = iand(psi_ref(k,2,i), nonactive_sorb(k,2))
    end do
    end do
    do k=1, N_int
      active_sorb(k,1) = iand(active_sorb(k,1), not(nonactive_sorb(k,1)))
      active_sorb(k,2) = iand(active_sorb(k,2), not(nonactive_sorb(k,2)))
    end do
  end if
  
  
  do i=1, N_det_non_ref
  do k=1, N_int
    det_noactive(k,1,i) = iand(psi_non_ref(k,1,i), not(active_sorb(k,1)))
    det_noactive(k,2,i) = iand(psi_non_ref(k,2,i), not(active_sorb(k,2))) 
  end do
  end do
  
  call sort_dets_ab(det_noactive, det_cepa0_idx, cepa0_shortcut, N_det_non_ref, N_int)
  
  do i=1,N_det_non_ref
    det_cepa0(:,:,i) = psi_non_ref(:,:,det_cepa0_idx(i))
  end do
  
  cepa0_shortcut(0) = 1
  cepa0_shortcut(1) = 1
  do i=2,N_det_non_ref
    if(.not. detEq(det_noactive(1,1,i), det_noactive(1,1,i-1), N_int)) then
      cepa0_shortcut(0) += 1
      cepa0_shortcut(cepa0_shortcut(0)) = i
    end if
  end do
  cepa0_shortcut(cepa0_shortcut(0)+1) = N_det_non_ref+1
  
  if(.true.) then 
  do i=1,cepa0_shortcut(0)
    n = cepa0_shortcut(i+1) - cepa0_shortcut(i)
    call sort_dets_ab(det_cepa0(1,1,cepa0_shortcut(i)), idx, shortcut, n, N_int)
    do k=1,n
      idx(k) = det_cepa0_idx(cepa0_shortcut(i)-1+idx(k))
    end do
    det_cepa0_idx(cepa0_shortcut(i):cepa0_shortcut(i)+n-1) = idx(:n)
  end do
  end if
  
  
  do i=1,N_det_ref
  do k=1, N_int
    det_ref_active(k,1,i) = iand(psi_ref(k,1,i), active_sorb(k,1))
    det_ref_active(k,2,i) = iand(psi_ref(k,2,i), active_sorb(k,2))
  end do
  end do
  
  do i=1,N_det_non_ref
  do k=1, N_int
    det_cepa0_active(k,1,i) = iand(psi_non_ref(k,1,det_cepa0_idx(i)), active_sorb(k,1))
    det_cepa0_active(k,2,i) = iand(psi_non_ref(k,2,det_cepa0_idx(i)), active_sorb(k,2))
  end do
  end do

  do i=1,N_det_non_ref
    if(.not. detEq(psi_non_ref(1,1,det_cepa0_idx(i)), det_cepa0(1,1,i),N_int)) stop "STOOOP"
  end do
  
  searchance = 0d0
  child_num = 0
  do J = 1, N_det_ref
    nlink(J) = 0
    do blok=1,cepa0_shortcut(0)
    do k=cepa0_shortcut(blok), cepa0_shortcut(blok+1)-1
      call get_excitation_degree(psi_ref(1,1,J),det_cepa0(1,1,k),degree,N_int)
      if(degree <= 2) then
        nlink(J) += 1
        linked(nlink(J),J) = k
        child_num(k, J) = nlink(J)
        blokMwen(nlink(J),J) = blok
        searchance(J) += 1d0 + log(dfloat(cepa0_shortcut(blok+1) - cepa0_shortcut(blok)))
      end if
    end do
    end do
  end do
  
  print *, "pre done"
END_PROVIDER

 
 BEGIN_PROVIDER [ double precision, delta_cas, (N_det_ref, N_det_ref, N_states) ]
  use bitmasks
  implicit none
  integer :: i,j,k
  double precision :: Hjk, Hki, Hij, pre(N_det_ref), wall
  integer :: i_state, degree, npre, ipre(N_det_ref), npres(N_det_ref)
  
  provide lambda_mrcc
  npres = 0
  delta_cas = 0d0
  call wall_time(wall)
  print *, "dcas ", wall
  do i_state = 1, N_states
    !!$OMP PARALLEL DO default(none) schedule(dynamic) private(pre,npre,ipre,j,k,Hjk,Hki,degree) shared(npres,lambda_mrcc,i_state, N_det_non_ref,psi_ref, psi_non_ref,N_int,delta_cas,N_det_ref)
    do k=1,N_det_non_ref
      if(lambda_mrcc(i_state, k) == 0d0) cycle
      npre = 0
      do i=1,N_det_ref
        call i_h_j(psi_non_ref(1,1,k),psi_ref(1,1,i), N_int,Hki)
        if(Hki /= 0d0) then
          !!$OMP ATOMIC
          npres(i) += 1
          npre += 1
          ipre(npre) = i
          pre(npre) = Hki
        end if
      end do
      
      
      do i=1,npre
      do j=1,i
        !!$OMP ATOMIC
        delta_cas(ipre(i),ipre(j),i_state) += pre(i) * pre(j) * lambda_mrcc(i_state, k)
      end do
      end do
    end do
    !!$OMP END PARALLEL DO
    print *, npres
    npre=0
    do i=1,N_det_ref
      npre += npres(i)
    end do
    print *, npre
    !stop
    do i=1,N_det_ref
    do j=1,i
        delta_cas(j,i,i_state) = delta_cas(i,j,i_state)
    end do
    end do 
  end do
  
  call wall_time(wall)
  print *, "dcas", wall
!   stop
 END_PROVIDER
 
 
logical function detEq(a,b,Nint)
   use bitmasks
   implicit none
   integer, intent(in) :: Nint
   integer(bit_kind), intent(in) :: a(Nint,2), b(Nint,2)
   integer :: ni, i
 
   detEq = .false.
   do i=1,2
   do ni=1,Nint
     if(a(ni,i) /= b(ni,i)) return
   end do
   end do
   detEq = .true.
end function


logical function isInCassd(a,Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer(bit_kind), intent(in) :: a(Nint,2)
  integer(bit_kind) :: inac, virt
  integer :: ni, i, deg

  
  isInCassd = .false.
     
  deg = 0
   do i=1,2
    do ni=1,Nint
      virt = iand(not(HF_bitmask(ni,i)), not(active_sorb(ni,i)))
      deg += popcnt(iand(virt, a(ni,i)))
      if(deg > 2) return
    end do
  end do
  
  deg = 0
  do i=1,2
    do ni=1,Nint
      inac = iand(HF_bitmask(ni,i), not(active_sorb(ni,i)))
      deg += popcnt(xor(iand(inac,a(ni,i)), inac))
      if(deg > 2) return
    end do
  end do
  isInCassd = .true.
end function


subroutine getHP(a,h,p,Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer(bit_kind), intent(in) :: a(Nint,2)
  integer, intent(out) :: h, p
  integer(bit_kind) :: inac, virt
  integer :: ni, i, deg

  
  !isInCassd = .false.
  h = 0
  p = 0
   
  deg = 0
  lp : do i=1,2
    do ni=1,Nint
      virt = iand(not(HF_bitmask(ni,i)), not(active_sorb(ni,i)))
      deg += popcnt(iand(virt, a(ni,i)))
      if(deg > 2) exit lp
    end do
  end do lp
  p = deg

  deg = 0
  lh : do i=1,2
    do ni=1,Nint
      inac = iand(HF_bitmask(ni,i), not(active_sorb(ni,i)))
      deg += popcnt(xor(iand(inac,a(ni,i)), inac))
      if(deg > 2) exit lh
    end do
  end do lh
  h = deg
  !isInCassd = .true.
end function

integer function detCmp(a,b,Nint)
   use bitmasks
   implicit none
   integer, intent(in) :: Nint
   integer(bit_kind), intent(in) :: a(Nint,2), b(Nint,2)
   integer :: ni, i
 
   detCmp = 0
   do i=1,2
   do ni=Nint,1,-1
   
     if(a(ni,i) < b(ni,i)) then
       detCmp = -1
       return
     else if(a(ni,i) > b(ni,i)) then
       detCmp = 1
       return
     end if
     
   end do
   end do
end function


integer function searchDet(dets, det, n, Nint)
  implicit none
  use bitmasks
  
  integer(bit_kind),intent(in) :: dets(Nint,2,n), det(Nint,2)
  integer, intent(in) :: nint, n
  integer :: l, h, c
  integer, external :: detCmp
  logical, external :: detEq

  !do l=1,n
  !  if(detEq(det(1,1), dets(1,1,l),Nint)) then
  !    searchDet = l
  !    return
  !  end if
  !end do
  !searchDet = -1
  !return


  l = 1
  h = n
  do while(.true.)
    searchDet = (l+h)/2
    c = detCmp(dets(1,1,searchDet), det(:,:), Nint)
    if(c == 0) return
    if(c == 1) then
      h = searchDet-1
    else
      l = searchDet+1
    end if
    if(l > h) then
      searchDet = -1
      return
    end if
    
  end do
end function


subroutine sort_det(key, idx, N_key, Nint)
  implicit none
  

  integer, intent(in)                   :: Nint, N_key
  integer(8),intent(inout)       :: key(Nint,2,N_key)
  integer,intent(out)                   :: idx(N_key)
  integer(8)                     :: tmp(Nint, 2)
  integer                               :: tmpidx,i,ni
  
  do i=1,N_key
    idx(i) = i
  end do
  
  do i=N_key/2,1,-1
    call tamiser(key, idx, i, N_key, Nint, N_key)
  end do
  
  do i=N_key,2,-1
    do ni=1,Nint
      tmp(ni,1) = key(ni,1,i)
      tmp(ni,2) = key(ni,2,i)
      key(ni,1,i) = key(ni,1,1)
      key(ni,2,i) = key(ni,2,1)
      key(ni,1,1) = tmp(ni,1)
      key(ni,2,1) = tmp(ni,2)
    enddo

    tmpidx = idx(i)
    idx(i) = idx(1)
    idx(1) = tmpidx
    call tamiser(key, idx, 1, i-1, Nint, N_key)
  end do
end subroutine 



 BEGIN_PROVIDER [ double precision, delta_mrcepa0_ij, (N_det_ref,N_det_non_ref,N_states) ]
&BEGIN_PROVIDER [ double precision, delta_mrcepa0_ii, (N_det_ref,N_states) ]
  use bitmasks
  implicit none
  
  integer                         :: i_state, i, i_I, J, k, degree, degree2, m, l, deg, ni
  integer                         :: p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_, sortRefIdx(N_det_ref)
  logical                         :: ok
  double precision                :: phase_iI, phase_Ik, phase_Jl, phase_IJ, phase_al, diI, hIi, hJi, delta_JI, dkI(1), HkI, ci_inv(1), dia_hla(1)
  double precision                :: contrib, HIIi, HJk, wall
  integer, dimension(0:2,2,2)     :: exc_iI, exc_Ik, exc_IJ
  integer(bit_kind)               :: det_tmp(N_int, 2), made_hole(N_int,2), made_particle(N_int,2), myActive(N_int,2), sortRef(N_int,2,N_det_ref)
  integer, allocatable            :: idx_sorted_bit(:)
  integer, external               :: get_index_in_psi_det_sorted_bit, searchDet
  logical, external               :: is_in_wavefunction, detEq
  
  integer :: II, blok
  integer*8, save :: notf = 0

  call wall_time(wall)
  print *, "cepa0", wall
  provide det_cepa0_active delta_cas lambda_mrcc
  provide mo_bielec_integrals_in_map
  allocate(idx_sorted_bit(N_det))
  
  sortRef(:,:,:) = det_ref_active(:,:,:)
  call sort_det(sortRef, sortRefIdx, N_det_ref, N_int)
  
  idx_sorted_bit(:) = -1
  do i=1,N_det_non_ref
    idx_sorted_bit(get_index_in_psi_det_sorted_bit(psi_non_ref(1,1,i), N_int)) = i
  enddo
    
  
  do i_state = 1, N_states
    delta_mrcepa0_ii(:,:) = 0d0
    delta_mrcepa0_ij(:,:,:) = 0d0

    !$OMP PARALLEL DO default(none) schedule(dynamic) shared(delta_mrcepa0_ij, delta_mrcepa0_ii)       &
    !$OMP private(m,i,II,J,k,degree,myActive,made_hole,made_particle,hjk,contrib)       &
    !$OMP shared(active_sorb, psi_non_ref, psi_non_ref_coef, psi_ref, psi_ref_coef, cepa0_shortcut, det_cepa0_active)     &
    !$OMP shared(N_det_ref, N_det_non_ref,N_int,det_cepa0_idx,lambda_mrcc,det_ref_active, delta_cas) &
    !$OMP shared(notf,i_state, sortRef, sortRefIdx)
    do blok=1,cepa0_shortcut(0)
    do i=cepa0_shortcut(blok), cepa0_shortcut(blok+1)-1
      do II=1,N_det_ref
        call get_excitation_degree(psi_ref(1,1,II),psi_non_ref(1,1,det_cepa0_idx(i)),degree,N_int)
        if (degree > 2 ) cycle
        
        do ni=1,N_int
          made_hole(ni,1) = iand(det_ref_active(ni,1,II), xor(det_cepa0_active(ni,1,i), det_ref_active(ni,1,II)))
          made_hole(ni,2) = iand(det_ref_active(ni,2,II), xor(det_cepa0_active(ni,2,i), det_ref_active(ni,2,II)))
          
          made_particle(ni,1) = iand(det_cepa0_active(ni,1,i), xor(det_cepa0_active(ni,1,i), det_ref_active(ni,1,II)))
          made_particle(ni,2) = iand(det_cepa0_active(ni,2,i), xor(det_cepa0_active(ni,2,i), det_ref_active(ni,2,II)))
        end do
      
        
  kloop: do k=cepa0_shortcut(blok), cepa0_shortcut(blok+1)-1 !i
          if(lambda_mrcc(i_state, det_cepa0_idx(k)) == 0d0) cycle
          
          do ni=1,N_int
            if(iand(made_hole(ni,1), det_cepa0_active(ni,1,k)) /= 0) cycle kloop
            if(iand(made_particle(ni,1), det_cepa0_active(ni,1,k)) /= made_particle(ni,1)) cycle kloop
            if(iand(made_hole(ni,2), det_cepa0_active(ni,2,k)) /= 0) cycle kloop
            if(iand(made_particle(ni,2), det_cepa0_active(ni,2,k)) /= made_particle(ni,2)) cycle kloop
          end do
          do ni=1,N_int
            myActive(ni,1) = xor(det_cepa0_active(ni,1,k), made_hole(ni,1))
            myActive(ni,1) = xor(myActive(ni,1), made_particle(ni,1))
            myActive(ni,2) = xor(det_cepa0_active(ni,2,k), made_hole(ni,2))
            myActive(ni,2) = xor(myActive(ni,2), made_particle(ni,2))
          end do
          
          j = searchDet(sortRef, myActive, N_det_ref, N_int)
          if(j == -1) then
           cycle
          end if
          j = sortRefIdx(j)
          !$OMP ATOMIC
          notf = notf+1
          !if(i/=k .and. dabs(psi_non_ref_coef(det_cepa0_idx(i),i_state)) < dabs(psi_non_ref_coef(det_cepa0_idx(k),i_state))) cycle
!           if(dabs(lambda_mrcc(i_state,det_cepa0_idx(i))) > dabs(lambda_mrcc(i_state,det_cepa0_idx(k)))) cycle
!           if(dabs(lambda_mrcc(i_state,det_cepa0_idx(i))) == dabs(lambda_mrcc(i_state,det_cepa0_idx(k))) .and. i < k) cycle
          !if(.not. j==II .and. dabs(psi_ref_coef(II,i_state)) < dabs(psi_ref_coef(j,i_state))) cycle
          
          call i_h_j(psi_non_ref(1,1,det_cepa0_idx(k)),psi_ref(1,1,J),N_int,HJk)
          contrib = delta_cas(II, J, i_state) * HJk * lambda_mrcc(i_state, det_cepa0_idx(k))
          !$OMP ATOMIC
          delta_mrcepa0_ij(J, det_cepa0_idx(i), i_state) += contrib
          
          if(dabs(psi_ref_coef(J,i_state)).ge.5.d-5) then
            !$OMP ATOMIC
            delta_mrcepa0_ii(J,i_state) -= contrib / psi_ref_coef(J, i_state) * psi_non_ref_coef(det_cepa0_idx(i),i_state)
          end if

        end do kloop
      end do
    end do
    end do
    !$OMP END PARALLEL DO
  end do
  deallocate(idx_sorted_bit)
  call wall_time(wall)
  print *, "cepa0", wall, notf
  !stop
END_PROVIDER


 BEGIN_PROVIDER [ double precision, delta_sub_ij, (N_det_ref,N_det_non_ref,N_states) ]
&BEGIN_PROVIDER [ double precision, delta_sub_ii, (N_det_ref, N_states) ]
  use bitmasks
  implicit none
  
  integer                         :: i_state, i, i_I, J, k, degree, degree2, l, deg, ni
  integer                         :: p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_
  logical                         :: ok
  double precision                :: phase_Ji, phase_Ik, phase_Ii
  double precision                :: contrib, delta_IJk, HJk, HIk, HIl
  integer, dimension(0:2,2,2)     :: exc_Ik, exc_Ji, exc_Ii
  integer(bit_kind)               :: det_tmp(N_int, 2), det_tmp2(N_int, 2)
  integer, allocatable            :: idx_sorted_bit(:)
  integer, external               :: get_index_in_psi_det_sorted_bit
  
  integer :: II, blok
  
  provide delta_cas lambda_mrcc
  allocate(idx_sorted_bit(N_det))
  idx_sorted_bit(:) = -1
  do i=1,N_det_non_ref
    idx_sorted_bit(get_index_in_psi_det_sorted_bit(psi_non_ref(1,1,i), N_int)) = i
  enddo
    
  do i_state = 1, N_states
    delta_sub_ij(:,:,:) = 0d0
    delta_sub_ii(:,:) = 0d0
    
    provide mo_bielec_integrals_in_map
    
    
    !$OMP PARALLEL DO default(none) schedule(dynamic,10) shared(delta_sub_ij, delta_sub_ii)       &
    !$OMP private(i, J, k, degree, degree2, l, deg, ni)       &
    !$OMP private(p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_)     &
    !$OMP private(ok, phase_Ji, phase_Ik, phase_Ii, contrib, delta_IJk, HJk, HIk, HIl, exc_Ik, exc_Ji, exc_Ii) &
    !$OMP private(det_tmp, det_tmp2, II, blok)    &
    !$OMP shared(idx_sorted_bit, N_det_non_ref, N_det_ref, N_int, psi_non_ref, psi_non_ref_coef, psi_ref, psi_ref_coef)   &
    !$OMP shared(i_state,lambda_mrcc, hf_bitmask, active_sorb)
    do i=1,N_det_non_ref
      if(mod(i,1000) == 0) print *, i, "/", N_det_non_ref
      do J=1,N_det_ref
        call get_excitation(psi_ref(1,1,J),psi_non_ref(1,1,i),exc_Ji,degree,phase_Ji,N_int)
        if(degree == -1) cycle
        
        
        do II=1,N_det_ref
          call apply_excitation(psi_ref(1,1,II),exc_Ji,det_tmp,ok,N_int)

          if(.not. ok) cycle
          l = get_index_in_psi_det_sorted_bit(det_tmp, N_int)
          if(l == 0) cycle
          l = idx_sorted_bit(l)
          
          call i_h_j(psi_ref(1,1,II), det_tmp, N_int, HIl)
          
          do k=1,N_det_non_ref
            if(lambda_mrcc(i_state, k) == 0d0) cycle
            call get_excitation(psi_ref(1,1,II),psi_non_ref(1,1,k),exc_Ik,degree2,phase_Ik,N_int)
            
            det_tmp(:,:) = 0_bit_kind
            det_tmp2(:,:) = 0_bit_kind
            
            ok = .true.
            do ni=1,N_int
              det_tmp(ni,1) = iand(xor(HF_bitmask(ni,1), psi_non_ref(ni,1,k)), not(active_sorb(ni,1)))
              det_tmp(ni,2) = iand(xor(HF_bitmask(ni,1), psi_non_ref(ni,1,i)), not(active_sorb(ni,1)))
              ok = ok .and. (popcnt(det_tmp(ni,1)) + popcnt(det_tmp(ni,2)) == popcnt(xor(det_tmp(ni,1), det_tmp(ni,2))))

              det_tmp(ni,1) = iand(xor(HF_bitmask(ni,2), psi_non_ref(ni,2,k)), not(active_sorb(ni,2)))
              det_tmp(ni,2) = iand(xor(HF_bitmask(ni,2), psi_non_ref(ni,2,i)), not(active_sorb(ni,2)))
              ok = ok .and. (popcnt(det_tmp(ni,1)) + popcnt(det_tmp(ni,2)) == popcnt(xor(det_tmp(ni,1), det_tmp(ni,2))))
            end do
            
            if(ok) cycle
            
            
            call i_h_j(psi_ref(1,1,J), psi_non_ref(1,1,k), N_int, HJk)
            call i_h_j(psi_ref(1,1,II), psi_non_ref(1,1,k), N_int, HIk)
            if(HJk == 0) cycle
            !assert HIk == 0
            delta_IJk = HJk * HIk * lambda_mrcc(i_state, k)
            call apply_excitation(psi_non_ref(1,1,i),exc_Ik,det_tmp,ok,N_int)
            if(ok) cycle
              contrib = delta_IJk * HIl * lambda_mrcc(i_state,l)   
            !$OMP ATOMIC
            delta_sub_ij(II, i, i_state) += contrib
            if(dabs(psi_ref_coef(II,i_state)).ge.5.d-5) then
              !$OMP ATOMIC
              delta_sub_ii(II,i_state) -= contrib / psi_ref_coef(II, i_state) * psi_non_ref_coef(l,i_state)
            endif
          end do
        end do
      end do
    end do
    !$OMP END PARALLEL DO
  end do
  deallocate(idx_sorted_bit)
END_PROVIDER


subroutine set_det_bit(det, p, s)
   implicit none
   integer(bit_kind),intent(inout) :: det(N_int, 2)
   integer, intent(in) :: p, s
   integer :: ni, pos
 
   ni = (p-1)/bit_kind_size + 1
   pos = mod(p-1, bit_kind_size)
   det(ni,s) = ibset(det(ni,s), pos)
end subroutine


BEGIN_PROVIDER [ double precision, h_, (N_det_ref,N_det_non_ref) ]
  integer :: i,j
  do i=1,N_det_ref
  do j=1,N_det_non_ref
    call i_h_j(psi_ref(1,1,i), psi_non_ref(1,1,j), N_int, h_(i,j))
  end do
  end do
END_PROVIDER


