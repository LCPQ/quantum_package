 
subroutine select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,buf)
  use bitmasks
  use selection_types
  implicit none
  
  integer, intent(in)            :: i_generator
  integer(bit_kind), intent(in)  :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)   :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)   :: E0(N_states)
  double precision, intent(inout) :: pt2(N_states)
  type(selection_buffer), intent(inout) :: buf
  
  double precision                :: mat(N_states, mo_tot_num, mo_tot_num)
  integer                         :: h1,h2,s1,s2,s3,i1,i2,ib,sp,k,i
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
          
          logical                        :: banned(mo_tot_num, mo_tot_num,2)
          logical                        :: bannedOrb(mo_tot_num, 2)
          
          banned = .false.
          bannedOrb(h1, s1) = .true.
          bannedOrb(h2, s2) = .true.
          
          bannedOrb(1:mo_tot_num, 1:2) = .true.
          do s3=1,2
            do i=1,N_particles(s3)
              bannedOrb(particle_list(i,s3), s3) = .false.
            enddo
          enddo
          
          
          call spot_isinwf(mask, psi_det_sorted, i_generator, N_det, banned, fullMatch)
          if(fullMatch) cycle
          if(sp /= 2) call spot_occupied(mask(1,1), bannedOrb(1,1))
          if(sp /= 1) call spot_occupied(mask(1,2), bannedOrb(1,2))
          
          mat = 0d0
          call splash_pq(mask, sp, psi_det_sorted, i_generator, N_det_selectors, bannedOrb, banned, mat)
          call fill_buffer_double(i_generator, sp, h1, h2, bannedOrb, banned, fock_diag_tmp, E0, pt2, mat, buf)
        enddo
      enddo
    enddo
  enddo
end subroutine


subroutine fill_buffer_double(i_generator, sp, h1, h2, bannedOrb, banned, fock_diag_tmp, E0, pt2, mat, buf)
  use bitmasks
  use selection_types
  implicit none
  
  integer, intent(in) :: i_generator, sp, h1, h2
  double precision, intent(in) :: mat(N_states, mo_tot_num, mo_tot_num)
  logical, intent(in) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num)
  double precision, intent(in)           :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)    :: E0(N_states)
  double precision, intent(inout) :: pt2(N_states) 
  type(selection_buffer), intent(inout) :: buf
  logical :: ok
  integer :: s1, s2, p1, p2, ib, j, istate
  integer(bit_kind) :: mask(N_int, 2), det(N_int, 2)
  double precision :: e_pert, delta_E, val, Hii, max_e_pert
  double precision, external :: diag_H_mat_elem_fock
  
  logical, external :: detEq
  
  
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
      max_e_pert = 0d0
      
      do istate=1,N_states
        delta_E = E0(istate) - Hii
        if (delta_E < 0.d0) then
          e_pert = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * val * val) - delta_E)
        else
          e_pert = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * val * val) - delta_E)
        endif
        pt2(istate) += e_pert
        if(dabs(e_pert) > dabs(max_e_pert)) max_e_pert = e_pert
      end do
      
      if(dabs(max_e_pert) > buf%mini) then
!         do j=1,buf%cur-1
!           if(detEq(buf%det(1,1,j), det, N_int)) then
!             print *, "tops"
!             print *, i_generator, s1, s2, h1, h2,p1,p2
!             stop
!           end if
!         end do
        call add_to_selection_buffer(buf, det, max_e_pert)
      end if
    end do
  end do
end subroutine


subroutine splash_pq(mask, sp, det, i_gen, N_sel, bannedOrb, banned, mat)
  use bitmasks
  implicit none

  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int, 2, N_sel)
  integer, intent(in) :: sp, i_gen, N_sel
  logical, intent(inout) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num, 2)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)

  integer :: i, j, k, l, h(0:2,2), p(0:4,2), nt
  integer(bit_kind) :: perMask(N_int, 2), mobMask(N_int, 2), negMask(N_int, 2)
  logical :: bandon
  
  mat = 0d0
  bandon = .false.
  
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
    
    !call assert(nt >= 2, irp_here//"qsd")
    if(i < i_gen) then
      if(nt == 4) call past_d2(banned, p, sp)
      if(nt == 3) call past_d1(bannedOrb, p)
      !call assert(nt /= 2, "should have been discarded")
    else
      if(i == i_gen) then
        bandon = .true.
        if(sp == 3) then
          banned(:,:,2) = transpose(banned(:,:,1))
        else
          do k=1,mo_tot_num
          do l=k+1,mo_tot_num
            banned(l,k,1) = banned(k,l,1)
          end do
          end do
        end if
      end if
      if(nt == 4) then
        call get_d2(det(1,1,i), psi_phasemask(1,1,i), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, i))
      else if(nt == 3) then
        call get_d1(det(1,1,i), psi_phasemask(1,1,i), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, i))
      else
        call get_d0(det(1,1,i), psi_phasemask(1,1,i), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, i))
      end if
    end if
  end do
  call assert(bandon, "BANDON")
end subroutine


subroutine get_d2(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: mask(N_int, 2), gen(N_int, 2)
  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num,2)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)
  integer, intent(in) :: h(0:2,2), p(0:4,2), sp
  
  double precision, external :: get_phase_bi, integral8
  
  integer :: i, j, tip, ma, mi, puti, putj
  integer :: h1, h2, p1, p2, i1, i2
  double precision :: hij, phase
  
  integer, parameter:: turn2d(2,3,4) = reshape((/0,0, 0,0, 0,0,  3,4, 0,0, 0,0,  2,4, 1,4, 0,0,  2,3, 1,3, 1,2 /), (/2,3,4/))
  integer, parameter :: turn2(2) = (/2, 1/)
  integer, parameter :: turn3(2,3) = reshape((/2,3,  1,3, 1,2/), (/2,3/))
  
  integer :: bant
  bant = 1

  tip = p(0,1) * p(0,2)
  !call assert(p(0,1) + p(0,2) == 4, irp_here//"df")
  ma = sp
  if(p(0,1) > p(0,2)) ma = 1
  if(p(0,1) < p(0,2)) ma = 2
  mi = mod(ma, 2) + 1
  
  !print *, "d2 SPtip", SP, tip
  if(sp == 3) then
    if(ma == 2) bant = 2
    
    if(tip == 3) then
      puti = p(1, mi)
      do i = 1, 3
        putj = p(i, ma)
        if(banned(putj,puti,bant)) cycle
        i1 = turn3(1,i)
        i2 = turn3(2,i)
        p1 = p(i1, ma)
        p2 = p(i2, ma)
        h1 = h(1, ma)
        h2 = h(2, ma)
        
        hij = (integral8(p1, p2, h1, h2) - integral8(p2,p1, h1, h2)) * get_phase_bi(phasemask, ma, ma, h1, p1, h2, p2)
        !call debug_hij(hij, gen, mask, mi, ma, puti, putj)
        if(ma == 1) then
          mat(:, putj, puti) += coefs * hij
        else
          mat(:, puti, putj) += coefs * hij
        end if
      end do
    else
      !call assert(tip == 4, "df")
      do i = 1,2
      do j = 1,2
        puti = p(i, 1)
        putj = p(j, 2)
        
        if(banned(puti,putj,bant)) cycle
        p1 = p(turn2(i), 1)
        p2 = p(turn2(j), 2)
        h1 = h(1,1)
        h2 = h(1,2)
        
        hij = integral8(p1, p2, h1, h2) * get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
        !call debug_hij(hij, gen, mask, 1, 2, puti, putj)
        mat(:, puti, putj) += coefs * hij
      end do
      end do
    end if

  else
    if(tip == 0) then
      h1 = h(1, ma)
      h2 = h(2, ma)
      do i=1,3
      puti = p(i, ma)
      do j=i+1,4
        putj = p(j, ma)
        if(banned(puti,putj,1)) cycle
        
        i1 = turn2d(1, i, j)
        i2 = turn2d(2, i, j)
        p1 = p(i1, ma)
        p2 = p(i2, ma)
        hij = (integral8(p1, p2, h1, h2) - integral8(p2,p1, h1, h2)) * get_phase_bi(phasemask, ma, ma, h1, p1, h2, p2)
        !call debug_hij(hij, gen, mask, ma, ma, puti, putj)
        mat(:, puti, putj) += coefs * hij
      end do
      end do
    else if(tip == 3) then
      h1 = h(1, mi)
      h2 = h(1, ma)
      p1 = p(1, mi)
      !call assert(ma == sp, "dldl")
      do i=1,3
        puti = p(turn3(1,i), ma)
        putj = p(turn3(2,i), ma)
        if(banned(puti,putj,1)) cycle
        p2 = p(i, ma)
        
        hij = integral8(p1, p2, h1, h2) * get_phase_bi(phasemask, mi, ma, h1, p1, h2, p2)
        !call debug_hij(hij, gen, mask, ma, ma, puti, putj)
        mat(:, min(puti, putj), max(puti, putj)) += coefs * hij
      end do
    else ! tip == 4
      !call assert(tip == 4, "qsdf")
      puti = p(1, sp)
      putj = p(2, sp)
      if(.not. banned(puti,putj,1)) then
        p1 = p(1, mi)
        p2 = p(2, mi)
        h1 = h(1, mi)
        h2 = h(2, mi)
        hij = (integral8(p1, p2, h1, h2) - integral8(p2,p1, h1, h2)) * get_phase_bi(phasemask, mi, mi, h1, p1, h2, p2)
          !call debug_hij(hij, gen, mask,ma,ma, puti, putj)
        mat(:, puti, putj) += coefs * hij
      end if
    end if
  end if
end subroutine


subroutine debug_hij(hij, gen, mask, s1, s2, p1, p2)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: gen(N_int,2), mask(N_int,2)
  double precision, intent(in) :: hij
  integer, intent(in) :: s1, s2, p1, p2
  integer(bit_kind) :: det(N_int,2)
  double precision :: hij_ref, phase_ref
  logical :: ok
  integer :: degree
  integer :: exc(0:2,2,2)

  call apply_particles(mask, s1, p1, s2, p2, det, ok, N_int)
  !call assert(ok, "nokey")
  call i_H_j_phase_out(gen,det,N_int,hij_ref,phase_ref,exc,degree)
  if(hij /= hij_ref) then
    print *, hij, hij_ref
    print *, s1, s2, p1, p2
    call debug_det(gen, N_int)
    call debug_det(mask, N_int)
    stop
  end if

  !  print *, "fourar",  hij, hij_ref,s1,s2
end function


subroutine get_d1(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: mask(N_int, 2), gen(N_int, 2)
  integer(1),intent(in) :: phasemask(N_int*bit_kind_size, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num,2)
  integer(bit_kind) :: det(N_int, 2)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)
  double precision :: hij, tmp_row(N_states, mo_tot_num), tmp_row2(N_states, mo_tot_num)
  double precision, external :: get_phase_bi, integral8
  
  logical :: lbanned(mo_tot_num, 2), ok
  integer :: puti, putj, ma, mi, s1, s2, i, i1, i2, j, hfix, pfix, h1, h2, p1, p2, ib
  
  integer, intent(in) :: h(0:2,2), p(0:4,2), sp
  
  integer, parameter :: turn2(2) = (/2,1/)
  integer, parameter :: turn3(2,3) = reshape((/2,3,  1,3, 1,2/), (/2,3/))
  
  integer :: bant
  
  
  lbanned = bannedOrb
    
  do i=1, p(0,1)
    lbanned(p(i,1), 1) = .true.
  end do
  do i=1, p(0,2)
    lbanned(p(i,2), 2) = .true.
  end do
  
  ma = 1
  if(p(0,2) >= 2) ma = 2
  mi = turn2(ma)
  
  bant = 1
  !print *, "d1 SP", sp, p(0,1)*p(0,2)

  if(sp == 3) then
    !move MA
    !call assert(p(0,1)*p(0,2) == 2, "ddmmm")
    if(ma == 2) bant = 2
    puti = p(1,mi)
    hfix = h(1,ma)
    p1 = p(1,ma)
    p2 = p(2,ma)
    if(.not. bannedOrb(puti, mi)) then
      tmp_row = 0d0
      do putj=1, hfix-1
        if(lbanned(putj, ma) .or. banned(putj, puti,bant)) cycle
        hij = (integral8(p1, p2, putj, hfix)-integral8(p2,p1,putj,hfix)) * get_phase_bi(phasemask, ma, ma, putj, p1, hfix, p2)
        !call debug_hij(hij, gen, mask, mi, ma, puti, putj)
        tmp_row(1:N_states,putj) += hij * coefs(1:N_states)
      end do
      do putj=hfix+1, mo_tot_num
        if(lbanned(putj, ma) .or. banned(putj, puti,bant)) cycle
        hij = (integral8(p1, p2, hfix, putj)-integral8(p2,p1,hfix,putj)) * get_phase_bi(phasemask, ma, ma, hfix, p1, putj, p2)
        !call debug_hij(hij, gen, mask, mi, ma, puti, putj)
        tmp_row(1:N_states,putj) += hij * coefs(1:N_states)
      end do

      if(ma == 1) then           
        mat(1:N_states,1:mo_tot_num,puti) += tmp_row(1:N_states,1:mo_tot_num)
      else
        mat(1:N_states,puti,1:mo_tot_num) += tmp_row(1:N_states,1:mo_tot_num)
      end if
    end if

    !MOVE MI
    pfix = p(1,mi)
    tmp_row = 0d0
    tmp_row2 = 0d0
    do puti=1,mo_tot_num
      if(lbanned(puti,mi)) cycle
      !p1 fixed
      putj = p1
      if(.not. banned(putj,puti,bant)) then
        hij = integral8(p2,pfix,hfix,puti) * get_phase_bi(phasemask, ma, mi, hfix, p2, puti, pfix)
        tmp_row(:,puti) += hij * coefs
      end if
      
      !call debug_hij(hij, gen, mask, mi, ma, puti, putj)
      putj = p2
      if(.not. banned(putj,puti,bant)) then
        hij = integral8(p1,pfix,hfix,puti) * get_phase_bi(phasemask, ma, mi, hfix, p1, puti, pfix)
        !call debug_hij(hij, gen, mask, mi, ma, puti, putj)
        tmp_row2(:,puti) += hij * coefs
      end if
    end do
    
    if(mi == 1) then
      mat(:,:,p1) += tmp_row(:,:)
      mat(:,:,p2) += tmp_row2(:,:)
    else
      mat(:,p1,:) += tmp_row(:,:)
      mat(:,p2,:) += tmp_row2(:,:)
    end if
  else
    if(p(0,ma) == 3) then
      do i=1,3
        hfix = h(1,ma)
        puti = p(i, ma)
        p1 = p(turn3(1,i), ma)
        p2 = p(turn3(2,i), ma)
        tmp_row = 0d0
        do putj=1,hfix-1
          if(lbanned(putj,ma) .or. banned(puti,putj,1)) cycle
          hij = (integral8(p1, p2, putj, hfix)-integral8(p2,p1,putj,hfix)) * get_phase_bi(phasemask, ma, ma, putj, p1, hfix, p2)
          !call debug_hij(hij, gen, mask, ma, ma, puti, putj)
          tmp_row(:,putj) += hij * coefs
        end do
        do putj=hfix+1,mo_tot_num
          if(lbanned(putj,ma) .or. banned(puti,putj,1)) cycle
          hij = (integral8(p1, p2, hfix, putj)-integral8(p2,p1,hfix,putj)) * get_phase_bi(phasemask, ma, ma, hfix, p1, putj, p2)
          !call debug_hij(hij, gen, mask, ma, ma, puti, putj)
          tmp_row(:,putj) += hij * coefs
        end do

        mat(:, :puti-1, puti) += tmp_row(:,:puti-1)
        mat(:, puti, puti:) += tmp_row(:,puti:)
      end do
    else
      !call assert(sp == ma, "sp == ma")
      hfix = h(1,mi)
      pfix = p(1,mi)
      p1 = p(1,ma)
      p2 = p(2,ma)
      tmp_row = 0d0
      tmp_row2 = 0d0
      do puti=1,mo_tot_num
        if(lbanned(puti,ma)) cycle
        putj = p2
        if(.not. banned(puti,putj,1)) then
          hij = integral8(pfix, p1, hfix, puti) * get_phase_bi(phasemask, mi, ma, hfix, pfix, puti, p1)
          !call debug_hij(hij, gen, mask, ma, ma, putj, puti)
          tmp_row(:,puti) += hij * coefs
        end if
        
        putj = p1
        if(.not. banned(puti,putj,1)) then
          hij = integral8(pfix, p2, hfix, puti) * get_phase_bi(phasemask, mi, ma, hfix, pfix, puti, p2)
          !call debug_hij(hij, gen, mask, ma, ma, putj, puti)
          tmp_row2(:,puti) += hij * coefs
        end if
      end do
      mat(:,:p2-1,p2) += tmp_row(:,:p2-1)
      mat(:,p2,p2:) += tmp_row(:,p2:)
      mat(:,:p1-1,p1) += tmp_row2(:,:p1-1)
      mat(:,p1,p1:) += tmp_row2(:,p1:)
    end if
  end if

 !! MONO
    if(sp == 3) then
      s1 = 1
      s2 = 2
    else
      s1 = sp
      s2 = sp
    end if

    do i1=1,p(0,s1)
      ib = 1
      if(s1 == s2) ib = i1+1
      do i2=ib,p(0,s2)
        p1 = p(i1,s1)
        p2 = p(i2,s2)
        if(bannedOrb(p1, s1) .or. bannedOrb(p2, s2) .or. banned(p1, p2, 1)) cycle
        call apply_particles(mask, s1, p1, s2, p2, det, ok, N_int)
        call i_h_j(gen, det, N_int, hij)
        mat(:, p1, p2) += coefs * hij
      end do
    end do
end subroutine




subroutine get_d0(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: gen(N_int, 2), mask(N_int, 2)
  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num,2)
  integer(bit_kind) :: det(N_int, 2)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)
  integer, intent(in) :: h(0:2,2), p(0:4,2), sp
  
  integer :: i, j, s, h1, h2, p1, p2, puti, putj
  double precision :: hij, phase
  double precision, external :: get_phase_bi, integral8
  logical :: ok
  
  integer :: bant
  bant = 1
  
  !print *, "d0 SP", sp

  if(sp == 3) then ! AB
    h1 = p(1,1)
    h2 = p(1,2)
    do p1=1, mo_tot_num
      if(bannedOrb(p1, 1)) cycle
      do p2=1, mo_tot_num
        if(bannedOrb(p2,2)) cycle
        if(banned(p1, p2, bant)) cycle ! rentable?
        if(p1 == h1 .or. p2 == h2) then
          call apply_particles(mask, 1,p1,2,p2, det, ok, N_int)
          !call assert(ok, "zsdq")
          call i_h_j(gen, det, N_int, hij)
          mat(:, p1, p2) += coefs(:) * hij
        else
          hij = integral8(p1, p2, h1, h2) * get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
          phase = get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
          !call debug_hij(hij, gen, mask, 1, 2, p1, p2)
          mat(:, p1, p2) += coefs(:) * hij
        end if
      end do
    end do
  else ! AA BB
    p1 = p(1,sp)
    p2 = p(2,sp)
    do puti=1, mo_tot_num
      if(bannedOrb(puti, sp)) cycle
      do putj=puti+1, mo_tot_num
        if(bannedOrb(putj, sp)) cycle
        if(banned(puti, putj, bant)) cycle ! rentable?
        if(puti == p1 .or. putj == p2 .or. puti == p2 .or. putj == p1) then
          call apply_particles(mask, sp,puti,sp,putj, det, ok, N_int)
          call i_h_j(gen, det, N_int, hij)
          mat(:, puti, putj) += coefs(:) * hij
        else
          hij = (integral8(p1, p2, puti, putj) -  integral8(p2, p1, puti, putj))* get_phase_bi(phasemask, sp, sp, puti, p1 , putj, p2)
          mat(:, puti, putj) += coefs(:) * hij
          !call debug_hij(hij, gen, mask, sp, sp, puti, putj)
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
      if(iand(det(j,2,i), mask(j,2)) /= mask(j, 2)) cycle genl
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
    !call assert(na + nb == 2, "oyo")
    !call assert(na == 1 .or. list(1) < list(2), "sqdsmmmm")
    banned(list(1), list(2)) = .true.
  end do genl
end subroutine

