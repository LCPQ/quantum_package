 

subroutine select_singles(i_gen,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,buf)
  use bitmasks
  use selection_types
  implicit none
  BEGIN_DOC
! Select determinants connected to i_det by H
  END_DOC
  integer, intent(in)             :: i_gen
  integer(bit_kind), intent(in)   :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)    :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)    :: E0(N_states)
  double precision, intent(inout) :: pt2(N_states)
  type(selection_buffer), intent(inout) :: buf
  
  double precision                :: vect(N_states, mo_tot_num)
  logical                         :: bannedOrb(mo_tot_num)
  integer                         :: i, k
  integer                         :: h1,h2,s1,s2,i1,i2,ib,sp
  integer(bit_kind)               :: hole(N_int,2), particle(N_int,2), mask(N_int, 2)
  logical                         :: fullMatch, ok
  
  
  do k=1,N_int
    hole    (k,1) = iand(psi_det_generators(k,1,i_gen), hole_mask(k,1))
    hole    (k,2) = iand(psi_det_generators(k,2,i_gen), hole_mask(k,2))
    particle(k,1) = iand(not(psi_det_generators(k,1,i_gen)), particle_mask(k,1))
    particle(k,2) = iand(not(psi_det_generators(k,2,i_gen)), particle_mask(k,2))
  enddo

  ! Create lists of holes and particles
  ! -----------------------------------

  integer                        :: N_holes(2), N_particles(2)
  integer                        :: hole_list(N_int*bit_kind_size,2)
  integer                        :: particle_list(N_int*bit_kind_size,2)

  call bitstring_to_list_ab(hole    , hole_list    , N_holes    , N_int)
  call bitstring_to_list_ab(particle, particle_list, N_particles, N_int)

  do sp=1,2
    do i=1, N_holes(sp)
      h1 = hole_list(i,sp)
      call apply_hole(psi_det_generators(1,1,i_gen), sp, h1, mask, ok, N_int)
      !call assert(ok, irp_here)
      bannedOrb = .false.
      bannedOrb(h1) = .true.
      call spot_hasBeen(mask, sp, psi_det_sorted, i_gen, N_det, bannedOrb, fullMatch)
      if(fullMatch) cycle
      call spot_occupied(mask(1,sp), bannedOrb)
      vect = 0d0
      call splash_p(mask, sp, psi_selectors(1,1,i_gen), psi_phasemask(1,1,i_gen), psi_selectors_coef_transp(1,i_gen), N_det_selectors - i_gen + 1, bannedOrb, vect)
      call fill_buffer_single(i_gen, sp, h1, bannedOrb, fock_diag_tmp, E0, pt2, vect, buf)
    end do
  enddo
end subroutine


subroutine fill_buffer_single(i_generator, sp, h1, bannedOrb, fock_diag_tmp, E0, pt2, vect, buf)
  use bitmasks
  use selection_types
  implicit none
  
  integer, intent(in) :: i_generator, sp, h1
  double precision, intent(in) :: vect(N_states, mo_tot_num)
  logical, intent(in) :: bannedOrb(mo_tot_num)
  double precision, intent(in)           :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)    :: E0(N_states)
  double precision, intent(inout) :: pt2(N_states)
  type(selection_buffer), intent(inout) :: buf
  logical :: ok
  integer :: s1, s2, p1, p2, ib
  integer(bit_kind) :: mask(N_int, 2), det(N_int, 2)
  double precision :: e_pert, delta_E, val, Hii
  double precision, external :: diag_H_mat_elem_fock
  
  if(N_states > 1) stop "fill_buffer_single N_states > 1"
  
  call apply_hole(psi_det_generators(1,1,i_generator), sp, h1, mask, ok, N_int)
  
  do p1=1,mo_tot_num
    if(bannedOrb(p1)) cycle
    if(vect(1, p1) == 0d0) cycle
    call apply_particle(mask, sp, p1, det, ok, N_int)
    val = vect(1, p1)
    
    Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),det,fock_diag_tmp,N_int)
    
    delta_E = E0(1) - Hii
    if (delta_E < 0.d0) then
      e_pert = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * val * val) - delta_E)
    else
      e_pert = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * val * val) - delta_E)
    endif
    pt2(1) += e_pert
    if(dabs(e_pert) > buf%mini) call add_to_selection_buffer(buf, det, e_pert)
  end do
end subroutine


subroutine splash_p(mask, sp, det, phasemask, coefs, N_sel, bannedOrb, vect)
  use bitmasks
  implicit none

  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int,2,N_sel), phasemask(N_int,2,N_sel)
  double precision, intent(in) :: coefs(N_states, N_sel)
  integer, intent(in) :: sp, N_sel
  logical, intent(inout) :: bannedOrb(mo_tot_num)
  double precision, intent(inout)     :: vect(N_states, mo_tot_num)

  integer :: i, j, h(0:2,2), p(0:3,2), nt
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

    if(nt > 3) cycle
      
    do j=1,N_int
      perMask(j,1) = iand(mask(j,1), not(det(j,1,i)))
      perMask(j,2) = iand(mask(j,2), not(det(j,2,i)))
    end do

    call bitstring_to_list(perMask(1,1), h(1,1), h(0,1), N_int)
    call bitstring_to_list(perMask(1,2), h(1,2), h(0,2), N_int)

    call bitstring_to_list(mobMask(1,1), p(1,1), p(0,1), N_int)
    call bitstring_to_list(mobMask(1,2), p(1,2), p(0,2), N_int)
    
    if(nt == 3) then
      call get_m2(det(1,1,i), phasemask(1,1,i), bannedOrb, vect, mask, h, p, sp, coefs(1, i))
    else if(nt == 2) then
      call get_m1(det(1,1,i), phasemask(1,1,i), bannedOrb, vect, mask, h, p, sp, coefs(1, i))
    else
      call get_m0(det(1,1,i), phasemask(1,1,i), bannedOrb, vect, mask, h, p, sp, coefs(1, i))
    end if
  end do
end subroutine


subroutine get_m2(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: gen(N_int, 2), phasemask(N_int, 2), mask(N_int, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: vect(N_states, mo_tot_num)
  integer, intent(in) :: sp, h(0:2, 2), p(0:3, 2)
  integer :: i, j, h1, h2, p1, p2, sfix, hfix, pfix, hmob, pmob, puti
  double precision :: hij
  double precision, external :: get_phase_bi
  
  integer, parameter :: turn3_2(2,3) = reshape((/2,3, 1,3, 1,2/), (/2,3/))
  integer, parameter :: turn2(2) = (/2,1/) 
  
  if(h(0,sp) == 2) then
    h1 = h(1, sp)
    h2 = h(2, sp)
    do i=1,3
      puti = p(i, sp)
      p1 = p(turn3_2(1,i), sp)
      p2 = p(turn3_2(2,i), sp)
      hij = integral8(p1, p2, h1, h2) - integral8(p2, p1, h1, h2)
      hij *= get_phase_bi(phasemask, sp, sp, h1, p1, h2, p2)
      !call debug_hij_mo(hij, gen, mask, sp, puti)
      vect(:, puti) += hij * coefs
    end do
  else if(h(0,sp) == 1) then
    sfix = turn2(sp)
    hfix = h(1,sfix)
    pfix = p(1,sfix)
    hmob = h(1,sp)
    do j=1,2
      puti = p(j, sp)
      pmob = p(turn2(j), sp)
      hij = integral8(pfix, pmob, hfix, hmob)
      hij *= get_phase_bi(phasemask, sp, sfix, hmob, pmob, hfix, pfix)
      !call debug_hij_mo(hij, gen, mask, sp, puti)
      vect(:, puti) += hij * coefs
    end do
  else
    puti = p(1,sp)
    sfix = turn2(sp)
    p1 = p(1,sfix)
    p2 = p(2,sfix)
    h1 = h(1,sfix)
    h2 = h(2,sfix)
    hij = (integral8(p1,p2,h1,h2) - integral8(p2,p1,h1,h2))
    hij *= get_phase_bi(phasemask, sfix, sfix, h1, p1, h2, p2)
    !call debug_hij_mo(hij, gen, mask, sp, puti)
    vect(:, puti) += hij * coefs
  end if
end subroutine



subroutine get_m1(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: gen(N_int, 2), phasemask(N_int, 2), mask(N_int, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: vect(N_states, mo_tot_num)
  integer, intent(in) :: sp, h(0:2, 2), p(0:3, 2)
  integer :: i, hole, p1, p2, sh
  logical :: ok, lbanned(mo_tot_num)
  integer(bit_kind) :: det(N_int, 2)
  double precision :: hij
  double precision, external :: get_phase_bi
  
  lbanned = bannedOrb
  sh = 1
  if(h(0,2) == 1) sh = 2
  hole = h(1, sh)
  lbanned(p(1,sp)) = .true.
  if(p(0,sp) == 2) lbanned(p(2,sp)) = .true.
  !print *, "SPm1", sp, sh
  
  p1 = p(1, sp)
  
  if(sp == sh) then
    p2 = p(2, sp)
    lbanned(p2) = .true.
    
    do i=1,hole-1
      if(lbanned(i)) cycle
      hij = (integral8(p1, p2, i, hole) - integral8(p2, p1, i, hole))
      hij *= get_phase_bi(phasemask, sp, sp, i, p1, hole, p2)
      !call debug_hij_mo(hij, gen, mask, sp, i)
      vect(:,i) += hij * coefs
    end do
    do i=hole+1,mo_tot_num
      if(lbanned(i)) cycle
      hij = (integral8(p1, p2, hole, i) - integral8(p2, p1, hole, i))
      hij *= get_phase_bi(phasemask, sp, sp, hole, p1, i, p2)
      call  debug_hij_mo(hij, gen, mask, sp, i)
      vect(:,i) += hij * coefs
    end do

    call apply_particle(mask, sp, p2, det, ok,  N_int)
    !call assert(ok, "OKE223")
    call i_h_j(gen, det, N_int, hij)
    vect(:, p2) += hij * coefs
  else
    p2 = p(1, sh)
    do i=1,mo_tot_num
      if(lbanned(i)) cycle
      hij = integral8(p1, p2, i, hole)
      hij *= get_phase_bi(phasemask, sp, sh, i, p1, hole, p2)
      !call debug_hij_mo(hij, gen, mask, sp, i)
      vect(:,i) += hij * coefs
    end do
  end if

  call apply_particle(mask, sp, p1, det, ok,  N_int)
  !call assert(ok, "OKQQE2")
  call i_h_j(gen, det, N_int, hij)
  vect(:, p1) += hij * coefs

  !print *, "endouille"
end subroutine


subroutine get_m0(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: gen(N_int, 2), phasemask(N_int, 2), mask(N_int, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: vect(N_states, mo_tot_num)
  integer, intent(in) :: sp, h(0:2, 2), p(0:3, 2)
  integer :: i
  logical :: ok, lbanned(mo_tot_num)
  integer(bit_kind) :: det(N_int, 2)
  double precision :: hij
  
  lbanned = bannedOrb
  lbanned(p(1,sp)) = .true.
  do i=1,mo_tot_num
    if(lbanned(i)) cycle
    call apply_particle(mask, sp, i, det, ok, N_int)
    !call assert(ok, "qsdo")
    call i_h_j(gen, det, N_int, hij)
    vect(:, i) += hij * coefs
  end do
end subroutine


subroutine spot_hasBeen(mask, sp, det, i_gen, N, banned, fullMatch)
  use bitmasks
  implicit none
  
  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int, 2, N)
  integer, intent(in) :: i_gen, N, sp
  logical, intent(inout) :: banned(mo_tot_num)
  logical, intent(out) :: fullMatch


  integer :: i, j, na, nb, list(3), nt
  integer(bit_kind) :: myMask(N_int, 2), negMask(N_int, 2)

  fullMatch = .false.

  do i=1,N_int
    negMask(i,1) = not(mask(i,1))
    negMask(i,2) = not(mask(i,2))
  end do

  genl : do i=1, N
    nt = 0
    
    do j=1, N_int
      myMask(j, 1) = iand(det(j, 1, i), negMask(j, 1))
      myMask(j, 2) = iand(det(j, 2, i), negMask(j, 2))
      nt += popcnt(myMask(j, 1)) + popcnt(myMask(j, 2))
    end do
    
    if(nt > 3) cycle
    
    if(nt <= 2 .and. i < i_gen) then
      fullMatch = .true.
      return
    end if
    
    call bitstring_to_list(myMask(1,sp), list(1), na, N_int)
    
    if(nt == 3 .and. i < i_gen) then
      do j=1,na
        banned(list(j)) = .true.
      end do
    else if(nt == 1 .and. na == 1) then
      banned(list(1)) = .true.
    end if
  end do genl
end subroutine



subroutine debug_hij_mo(hij, gen, mask, s1, p1)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: gen(N_int,2), mask(N_int,2)
  double precision, intent(in) :: hij
  integer, intent(in) :: s1, p1
  integer(bit_kind) :: det(N_int,2)
  double precision :: hij_ref, phase_ref
  logical :: ok
  integer :: degree
  integer :: exc(0:2,2,2)
  logical, external :: detEq

  call apply_particle(mask, s1, p1, det, ok, N_int)
  !call assert(ok, "nokey_mo")
  !call assert(.not. detEq(det, gen, N_int), "Hii ...")
  call i_H_j_phase_out(gen,det,N_int,hij_ref,phase_ref,exc,degree)
  if(hij /= hij_ref) then
    print *, hij, hij_ref
    print *, s1, p1
    call debug_det(gen, N_int)
    call debug_det(mask, N_int)
    call debug_det(det, N_int)
    stop
    end if
end function

