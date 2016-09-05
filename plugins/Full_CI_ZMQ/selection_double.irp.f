 
subroutine select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,buf)
  use bitmasks
  use selection_types
  implicit none
  
  integer, intent(in)             :: i_generator
  integer(bit_kind), intent(in)   :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)           :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)    :: E0(N_states)
  double precision, intent(inout) :: pt2(N_states)
  type(selection_buffer), intent(inout) :: buf
  
  double precision                :: mat(N_states, mo_tot_num, mo_tot_num)
  integer                         :: h1,h2,s1,s2,i1,i2,ib,sp,k
  integer(bit_kind)               :: hole(N_int,2), particle(N_int,2), mask(N_int, 2)
  logical                         :: fullMatch, ok
  
  do k=1,N_int
    hole    (k,1) = iand(psi_det_generators(k,1,i_generator), hole_mask(k,1))
    hole    (k,2) = iand(psi_det_generators(k,2,i_generator), hole_mask(k,2))
    particle(k,1) = iand(not(psi_det_generators(k,1,i_generator)), particle_mask(k,1))
    particle(k,2) = iand(not(psi_det_generators(k,2,i_generator)), particle_mask(k,2))
  enddo

  integer                        :: N_holes(2), N_particles(2)
  integer                        :: hole_list(N_int*bit_kind_size,2)
  integer                        :: particle_list(N_int*bit_kind_size,2)

  call bitstring_to_list_ab(hole    , hole_list    , N_holes    , N_int)
  call bitstring_to_list_ab(particle, particle_list, N_particles, N_int)


  do s1=1,2
  do s2=s1,2
    sp = s1
    if(s1 /= s2) sp = 3
    do i1=N_holes(s1),1,-1   ! Generate low excitations first
    ib = 1
    if(s1 == s2) ib = i1+1
    do i2=N_holes(s2),ib,-1   ! Generate low excitations first
      h1 = hole_list(i1,s1)
      h2 = hole_list(i2,s2)
      call apply_holes(psi_det_generators(1,1,i_generator), s1,h1,s2,h2, mask, ok, N_int)
      
      logical :: banned(mo_tot_num, mo_tot_num)
      logical :: bannedOrb(mo_tot_num, 2)

      banned = .false.
      bannedOrb = .false.

      call spot_isinwf(mask, psi_det_sorted, i_generator, N_det, banned, fullMatch)
      if(fullMatch) cycle
      call spot_occupied(mask, bannedOrb)
      mat = 0d0
      call splash_pq(mask, sp, psi_det_sorted, i_generator, N_det_selectors, bannedOrb, banned, mat)
      
      call fill_buffer_double(i_generator, sp, h1, h2, banned, bannedOrb, fock_diag_tmp, E0, mat, buf)
    end do
    end do
  end do
  end do
end subroutine


subroutine fill_buffer_double(i_generator, sp, h1, h2, bannedOrb, banned, fock_diag_tmp, E0, mat, buf)
  use bitmasks
  use selection_types
  implicit none
  
  integer, intent(in) :: i_generator, sp, h1, h2
  double precision, intent(in) :: mat(N_states, mo_tot_num, mo_tot_num)
  logical, intent(in) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num)
  double precision, intent(in)           :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)    :: E0(N_states)
  type(selection_buffer), intent(inout) :: buf
  logical :: ok
  integer :: s1, s2, p1, p2, ib
  integer(bit_kind) :: mask(N_int, 2), det(N_int, 2)
  double precision :: e_pert, delta_E, val, Hii
  double precision, external :: diag_H_mat_elem_fock
  
  if(N_states > 1) stop "fill_buffer_double N_states > 1"
  
  if(sp == 3) then
    s1 = 1
    s2 = 2
  else
    s1 = sp
    s2 = sp
  end if
  
  call apply_holes(psi_det_generators(1,1,i_generator), s1, h1, s2, h2, mask, ok, N_int)
  
  do p1=1,mo_tot_num
    if(bannedOrb(p1, s1)) cycle
    ib = 1
    if(sp /= 3) ib = p1+1
    do p2=ib,mo_tot_num
      if(bannedOrb(p2, s2)) cycle
      if(banned(p1,p2)) cycle
      if(mat(1, p1, p2) == 0d0) cycle
      call apply_particles(mask, s1, p1, s2, p2, det, ok, N_int)
      val = mat(1, p1, p2)
      
      Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),det,fock_diag_tmp,N_int)
    
      delta_E = E0(1) - Hii
      if (delta_E < 0.d0) then
        e_pert = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * val * val) - delta_E)
      else
        e_pert = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * val * val) - delta_E)
      endif
      if(dabs(e_pert) > buf%mini) call add_to_selection_buffer(buf, det, e_pert)
    end do
  end do
end subroutine


subroutine splash_pq(mask, sp, det, i_gen, N_sel, bannedOrb, banned, mat)
  use bitmasks
  implicit none

  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int, 2, N_sel)
  integer, intent(in) :: sp, i_gen, N_sel
  logical, intent(inout) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num) ! intent out

  integer :: i, j, h(0:2,2), p(0:4,2), nt
  integer(bit_kind) :: perMask(N_int, 2), mobMask(N_int, 2), negMask(N_int, 2)
  
  
  do i=1,N_int
    negMask(i,1) = not(mask(i,1))
    negMask(i,2) = not(mask(i,2))
  end do

  do i=1, N_sel
    nt = 0
    do j=1,N_int
      mobMask(j,1) = iand(negMask(j,1), det(j,1,i))
      mobMask(j,2) = iand(negMask(j,2), det(j,2,i))
      nt += popcnt(mobMask(j, 1)) + popcnt(mobMask(j, 2))
    end do

    if(nt > 4) cycle

    do j=1,N_int
      perMask(j,1) = iand(mask(j,1), not(det(j,1,i)))
      perMask(j,2) = iand(mask(j,2), not(det(j,2,i)))
    end do

    call bitstring_to_list(perMask(1,1), h(1,1), h(0,1), N_int)
    call bitstring_to_list(perMask(1,2), h(1,2), h(0,2), N_int)

    call bitstring_to_list(mobMask(1,1), p(1,1), p(0,1), N_int)
    call bitstring_to_list(mobMask(1,2), p(1,2), p(0,2), N_int)

    if(i < i_gen) then
      if(nt == 4) call past_d2(banned, p, sp)
      if(nt == 3) call past_d1(bannedOrb, p)
    else
      if(i == i_gen) mat = 0d0
      if(nt == 4) then
          call get_d2(det(1,1,i), psi_phasemask(1,1,i), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, i))
      else if(nt == 3) then
          call get_d1(det(1,1,i), psi_phasemask(1,1,i), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, i))
      else
          call get_d0(det(1,1,i), psi_phasemask(1,1,i), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, i))
      end if
    end if
  end do
end subroutine


subroutine get_d2(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: mask(N_int, 2), gen(N_int, 2), phasemask(N_int, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: mat(mo_tot_num, mo_tot_num, N_states)
  integer, intent(in) :: h(0:2,2), p(0:4,2), sp
  
  double precision, external :: get_phase_bi
  
  integer :: i, j, tip, ma, mi, puti, putj
  integer :: h1, h2, p1, p2
  double precision :: hij, phase
  
  tip = p(0,1) * p(0,2)
  ma = sp
  if(p(0,1) > p(0,2)) ma = 1
  if(p(0,1) < p(0,2)) ma = 2
  mi = mod(ma, 2) + 1

  if(sp == 3) then
    if(tip == 3) then
      putj = p(1, 2)
      do i = 1, 3
        puti = p(i, ma)

        p1 = p(mod(i, 3) + 1, ma)
        p2 = p(mod(i+1, 3) + 1, ma)

        hij = (integral8(p1, p2, h1, h2) - integral8(p2,p1, h1, h2)) * get_phase_bi(phasemask, ma, ma, h1, p1, h2, p2)

        if(ma == 1) then
          mat(:, puti, putj) += coefs * hij
        else
          mat(:, putj, puti) += coefs * hij
        end if
      end do
    else ! tip == 4
      do i = 1,2
      do j = 1,2
        puti = p(i, 1)
        putj = p(j, 2)
        p1 = p(mod(i, 2) + 1, 1)
        p2 = p(mod(j, 2) + 1, 2)

        hij = integral8(p1, p2, h1, h2) * get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)

        mat(:, puti, putj) += coefs * hij
      end do
      end do
    end if

  else !! AA/BB

    if(tip == 0) then
      do i=1,3
      puti = p(i, ma)
      do j=i+1,4
        putj = p(j, ma)
        if(j == i+1) then
          p1 = p(mod(j, 4) + 1, ma)
          p2 = p(mod(j+1, 4) + 1, ma)
        else if(j == i+2) then
          p1 = p(mod(i, 4) + 1, ma)
          p2 = p(mod(j, 4) + 1, ma)
        else
          p1 = 2
          p2 = 3
        end if

        hij = (integral8(p1, p2, h1, h2) - integral8(p2,p1, h1, h2)) * get_phase_bi(phasemask, ma, ma, h1, p1, h2, p2)
        mat(:, puti, putj) += coefs * hij
      end do
      end do
    else if(tip == 3) then
      p2 = p(1, mi)
      do i=1,3
        p1 = p(i, ma)
        puti = p(mod(i, 3) + 1, ma)
        putj = p(mod(i+1, 3) + 1, ma)
        hij = integral8(p1, p2, h1, h2) * get_phase_bi(phasemask, ma, mi, h1, p1, h2, p2)
        mat(:, min(puti, putj), max(puti, putj)) += coefs * hij
      end do
    else ! tip == 4
      puti = p(1, sp)
      putj = p(2, sp)
      p1 = p(1, mi)
      p2 = p(2, mi)
      hij = (integral8(p1, p2, h1, h2) - integral8(p2,p1, h1, h2)) * get_phase_bi(phasemask, mi, mi, h1, p1, h2, p2)
      mat(:, puti, putj) += coefs * hij
    end if
  end if
end subroutine



subroutine get_d1(gen, phasemask, bannedOrb, banned, mat, mask, ho, pa, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: mask(N_int, 2), gen(N_int, 2), phasemask(N_int, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num)
  integer(bit_kind) :: det(N_int, 2)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)
  double precision :: hij, tmp_row(N_states, mo_tot_num)
  double precision, external :: get_phase_bi
  
  logical :: lbanned(mo_tot_num, 2), ok
  integer :: ms, i, i1, i2, j, hole, tmp, s(3), p(3)
  
  integer, intent(in) :: ho(0:2,2), pa(0:4,2), sp

  do i = 1, pa(0,1)
    s(i) = 1
    p(i) = pa(i, 1)
  end do

  j = i

  do i = 1, pa(0,2)
    s(j) = 2
    p(j) = pa(i, 2)
    j += 1
  end do

  if(ho(0,1) == 1) then
    hole = ho(1,1)
  else
    hole = ho(1,2)
  end if

  lbanned = bannedOrb

  do i=1, 3
    lbanned(p(i), s(i)) = .true.
  end do

  do i=1, 3
    if(lbanned(p(i), s(i))) cycle
    if(sp /= 3 .and. sp /= s(i)) cycle
    ms = sp
    if(sp == 3) ms = mod(s(i), 2) + 1
    i1 = mod(i,3)+1
    i2 = mod(i+1,3)+1

    if(s(i1) /= s(i2)) then
      if(s(i1) /= ms) then
        tmp = i1
        i1 = i2
        i2 = tmp
      end if
      tmp_row = 0d0
      do j=1,mo_tot_num
        if(lbanned(j, s(i))) cycle
        tmp_row(:, j) += coefs * integral8(p(i1), p(i2), j, hole) * get_phase_bi(phasemask, 1, 2, j, p(i1), hole, p(i2))
      end do
      if(ms == 1) then
        mat(:, :, p(i)) += tmp_row
      else
        mat(:, p(i), :) += tmp_row
      end if
    else
      do j=1,mo_tot_num
        if(lbanned(j, s(i))) cycle
        tmp_row(:, j) += coefs * (integral8(p(i1), p(i2), j, hole) - integral8(p(i2), p(i1), j, hole)) * get_phase_bi(phasemask, 1, 2, j, p(i1), hole, p(i2))
      end do
      mat(:, :p(i), p(i)) += tmp_row(:, :p(i))
      mat(:, p(i), p(i):) += tmp_row(:, p(i):)
    end if
  end do
 !! MONO
  do i=1, 2
    do j=i+1,3
      if(bannedOrb(p(i), s(i)) .or. bannedOrb(p(j), s(j))) cycle
      if((s(i) /= s(j) .or. sp /= s(i)) .and. (s(i) == s(j) .or. sp /= 3)) cycle
      call apply_particles(mask, s(i), p(i), s(j), p(j), det, ok, N_int)
      call i_h_j(gen, det, N_int, hij)

      if(s(i) == s(j)) then
        mat(:, p(i), p(j)) += coefs * hij
      else if(s(i) == 1) then
        mat(:, p(i), p(j)) += coefs * hij
      else
        mat(:, p(j), p(i)) += coefs * hij
      end if
    end do
  end do
end subroutine




subroutine get_d0(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: gen(N_int, 2), mask(N_int, 2), phasemask(N_int, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num), banned(mo_tot_num, mo_tot_num)
  integer(bit_kind) :: det(N_int, 2)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)
  integer, intent(in) :: h(0:2,2), p(0:4,2), sp
  
  integer :: i, j, s, h1, h2, p1, p2
  double precision :: hij
  double precision, external :: get_phase_bi
  logical :: ok
  
  if(sp == 3) then ! AB
    h1 = p(1,1)
    h2 = p(1,2)
    do p1=1, mo_tot_num
      if(bannedOrb(i)) cycle
      do p2=1, mo_tot_num
        if(bannedOrb(j)) cycle
        if(banned(i, j)) cycle ! rentable?
        if(p1 == h1 .or. p2 == h2) then
          call apply_particles(mask, 1,p1,2,p2, det, ok, N_int)
          call i_h_j(gen, det, N_int, hij)
          mat(:, p1, p2) += coefs * hij
        else
          mat(:, p1, p2) += coefs * integral8(p1, p2, h1, h2) * get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
        end if
      end do
    end do
  else ! AA BB
    s = 1
    if(p(0,2) == 2) s =2
    h1 = p(1,s)
    h2 = p(2,s)
    do p1=1, mo_tot_num
      if(bannedOrb(i)) cycle
      do p2=p1+1, mo_tot_num
        if(bannedOrb(j)) cycle
        if(banned(i, j)) cycle ! rentable?
        if(p1 == h1 .or. p2 == h2 .or. p1 == h2 .or. p2 == h1) then
          call apply_particles(mask, s,p1,s,p2, det, ok, N_int)
          ASSERT(ok)
          call i_h_j(gen, det, N_int, hij)
          mat(:, p1, p2) += coefs * hij
        else
          mat(:, p1, p2) += coefs * (integral8(p1, p2, h1, h2) -  integral8(p2, p1, h1, h2))* get_phase_bi(phasemask, s, s, h1, p1, h2, p2)
        end if
      end do
    end do
  end if
end subroutine
 

subroutine past_d1(bannedOrb, p)
  use bitmasks
  implicit none

  logical, intent(inout) :: bannedOrb(mo_tot_num, 2)
  integer, intent(in) :: p(0:4, 2)
  integer :: i,s

  do s = 1, 2
    do i = 1, p(0, s)
      bannedOrb(p(i, s), s) = .true.
    end do
  end do
end subroutine


subroutine past_d2(banned, p, sp)
  use bitmasks
  implicit none

  logical, intent(inout) :: banned(mo_tot_num, mo_tot_num)
  integer, intent(in) :: p(0:4, 2), sp
  integer :: i,j

  if(sp == 3) then
    do i=1,p(0,1)
      do j=1,p(0,2)
        banned(p(i,1), p(j,2)) = .true.
      end do
    end do
  else
    do i=1,p(0, sp)
      do j=1,i-1
        banned(p(j,sp), p(i,sp)) = .true.
        banned(p(i,sp), p(j,sp)) = .true.
      end do
    end do
  end if
end subroutine



subroutine spot_isinwf(mask, det, i_gen, N, banned, fullMatch)
  use bitmasks
  implicit none
  
  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int, 2, N)
  integer, intent(in) :: i_gen, N
  logical, intent(inout) :: banned(mo_tot_num, mo_tot_num)
  logical, intent(out) :: fullMatch


  integer :: i, j, na, nb, list(3)
  integer(bit_kind) :: myMask(N_int, 2), negMask(N_int, 2)

  fullMatch = .false.

  do i=1,N_int
    negMask(i,1) = not(mask(i,1))
    negMask(i,2) = not(mask(i,2))
  end do

  genl : do i=1, N
    do j=1, N_int
      if(iand(det(j,1,i), mask(j,1)) /= mask(j, 1)) cycle genl
      if(iand(det(j,2,i), mask(j,2)) /= mask(j, 1)) cycle genl
    end do

    if(i < i_gen) then
      fullMatch = .true.
      return
    end if

    do j=1, N_int
      myMask(j, 1) = iand(det(j, 1, i), negMask(j, 1))
      myMask(j, 2) = iand(det(j, 2, i), negMask(j, 2))
    end do

    call bitstring_to_list(myMask(1,1), list(1), na, N_int)
    call bitstring_to_list(myMask(1,2), list(na+1), nb, N_int)
    
    banned(list(1), list(2)) = .true.
  end do genl
end subroutine

