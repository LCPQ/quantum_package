use bitmasks


BEGIN_PROVIDER [ integer(1), psi_phasemask, (N_int*bit_kind_size, 2, N_det)]
  use bitmasks
  implicit none
  
  integer :: i
  do i=1, N_det
    call get_mask_phase(psi_selectors(1,1,i), psi_phasemask(1,1,i))
  end do
END_PROVIDER


subroutine assert(cond, msg)
  character(*), intent(in) :: msg
  logical, intent(in) :: cond
  
  if(.not. cond) then
    print *, "assert fail: "//msg
    stop
  end if
end subroutine


subroutine get_mask_phase(det, phasemask)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: det(N_int, 2)
  integer(1), intent(out) :: phasemask(N_int*bit_kind_size, 2)
  integer :: s, ni, i
  logical :: change

  phasemask = 0_1
  do s=1,2
    change = .false.
    do ni=1,N_int
      do i=0,bit_kind_size-1
        if(BTEST(det(ni, s), i)) change = .not. change
          if(change) phasemask((ni-1)*bit_kind_size + i + 1, s) = 1_1
      end do
    end do
  end do
end subroutine


subroutine select_connected(i_generator,E0,pt2,b)
  use bitmasks
  use selection_types
  implicit none
  integer, intent(in)            :: i_generator
  type(selection_buffer), intent(inout) :: b
  double precision, intent(inout)  :: pt2(N_states)
  integer :: k,l
  double precision, intent(in)   :: E0(N_states)

  integer(bit_kind)              :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision               :: fock_diag_tmp(2,mo_tot_num+1)
  
  call build_fock_tmp(fock_diag_tmp,psi_det_generators(1,1,i_generator),N_int)

  do l=1,N_generators_bitmask
    do k=1,N_int
      hole_mask(k,1) = iand(generators_bitmask(k,1,s_hole,l), psi_det_generators(k,1,i_generator))
      hole_mask(k,2) = iand(generators_bitmask(k,2,s_hole,l), psi_det_generators(k,2,i_generator))
      particle_mask(k,1) = iand(generators_bitmask(k,1,s_part,l), not(psi_det_generators(k,1,i_generator)) )
      particle_mask(k,2) = iand(generators_bitmask(k,2,s_part,l), not(psi_det_generators(k,2,i_generator)) )

    enddo
    call select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
    call select_singles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
  enddo
end subroutine


double precision function get_phase_bi(phasemask, s1, s2, h1, p1, h2, p2)
  use bitmasks
  implicit none

  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
  integer, intent(in) :: s1, s2, h1, h2, p1, p2
  logical :: change
  integer(1) :: np
  double precision, parameter :: res(0:1) = (/1d0, -1d0/)

  np = phasemask(h1,s1) + phasemask(p1,s1) + phasemask(h2,s2) + phasemask(p2,s2)
  if(p1 < h1) np = np + 1_1
  if(p2 < h2) np = np + 1_1
  
  if(s1 == s2 .and. max(h1, p1) > min(h2, p2)) np = np + 1_1
  get_phase_bi = res(iand(np,1_1))
end function



! Selection single
! ----------------

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
  integer                         :: i, j, k
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
      bannedOrb = .true.
      do j=1,N_particles(sp)
        bannedOrb(particle_list(j, sp)) = .false.
      end do
      call spot_hasBeen(mask, sp, psi_selectors, i_gen, N_det, bannedOrb, fullMatch)
      if(fullMatch) cycle
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
  integer :: s1, s2, p1, p2, ib, istate
  integer(bit_kind) :: mask(N_int, 2), det(N_int, 2)
  double precision :: e_pert, delta_E, val, Hii, max_e_pert, tmp
  double precision, external :: diag_H_mat_elem_fock
  
  
  call apply_hole(psi_det_generators(1,1,i_generator), sp, h1, mask, ok, N_int)
  
  do p1=1,mo_tot_num
    if(bannedOrb(p1)) cycle
    if(vect(1, p1) == 0d0) cycle
    call apply_particle(mask, sp, p1, det, ok, N_int)
    
    Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),det,fock_diag_tmp,N_int)
    max_e_pert = 0d0
    
    do istate=1,N_states
      val = vect(istate, p1) + vect(istate, p1)
      delta_E = E0(istate) - Hii
      tmp = dsqrt(delta_E * delta_E + val * val)
      if (delta_E < 0.d0) then
        tmp = -tmp
      endif
      e_pert = 0.5d0 * ( tmp - delta_E)
      pt2(istate) += e_pert
      if(dabs(e_pert) > dabs(max_e_pert)) max_e_pert = e_pert
    end do
    
    if(dabs(max_e_pert) > buf%mini) then
       call add_to_selection_buffer(buf, det, max_e_pert)
     endif
  end do
end subroutine


subroutine splash_p(mask, sp, det, phasemask, coefs, N_sel, bannedOrb, vect)
  use bitmasks
  implicit none

  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int,2,N_sel)
  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2, N_sel)
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
  
  integer(bit_kind), intent(in) :: gen(N_int, 2), mask(N_int, 2)
  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: vect(N_states, mo_tot_num)
  integer, intent(in) :: sp, h(0:2, 2), p(0:3, 2)
  integer :: i, j, h1, h2, p1, p2, sfix, hfix, pfix, hmob, pmob, puti
  double precision :: hij
  double precision, external :: get_phase_bi, mo_bielec_integral
  
  integer, parameter :: turn3_2(2,3) = reshape((/2,3, 1,3, 1,2/), (/2,3/))
  integer, parameter :: turn2(2) = (/2,1/) 
  
  if(h(0,sp) == 2) then
    h1 = h(1, sp)
    h2 = h(2, sp)
    do i=1,3
      puti = p(i, sp)
      if(bannedOrb(puti)) cycle
      p1 = p(turn3_2(1,i), sp)
      p2 = p(turn3_2(2,i), sp)
      hij = mo_bielec_integral(p1, p2, h1, h2) - mo_bielec_integral(p2, p1, h1, h2)
      hij *= get_phase_bi(phasemask, sp, sp, h1, p1, h2, p2)
      vect(:, puti) += hij * coefs
    end do
  else if(h(0,sp) == 1) then
    sfix = turn2(sp)
    hfix = h(1,sfix)
    pfix = p(1,sfix)
    hmob = h(1,sp)
    do j=1,2
      puti = p(j, sp)
      if(bannedOrb(puti)) cycle
      pmob = p(turn2(j), sp)
      hij = mo_bielec_integral(pfix, pmob, hfix, hmob)
      hij *= get_phase_bi(phasemask, sp, sfix, hmob, pmob, hfix, pfix)
      vect(:, puti) += hij * coefs
    end do
  else
    puti = p(1,sp)
    if(.not. bannedOrb(puti)) then
      sfix = turn2(sp)
      p1 = p(1,sfix)
      p2 = p(2,sfix)
      h1 = h(1,sfix)
      h2 = h(2,sfix)
      hij = (mo_bielec_integral(p1,p2,h1,h2) - mo_bielec_integral(p2,p1,h1,h2))
      hij *= get_phase_bi(phasemask, sfix, sfix, h1, p1, h2, p2)
      vect(:, puti) += hij * coefs
    end if
  end if
end subroutine



subroutine get_m1(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: gen(N_int, 2), mask(N_int, 2)
  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
  logical, intent(in) :: bannedOrb(mo_tot_num)
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: vect(N_states, mo_tot_num)
  integer, intent(in) :: sp, h(0:2, 2), p(0:3, 2)
  integer :: i, hole, p1, p2, sh
  logical :: ok, lbanned(mo_tot_num)
  integer(bit_kind) :: det(N_int, 2)
  double precision :: hij
  double precision, external :: get_phase_bi, mo_bielec_integral
  
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
      hij = (mo_bielec_integral(p1, p2, i, hole) - mo_bielec_integral(p2, p1, i, hole))
      hij *= get_phase_bi(phasemask, sp, sp, i, p1, hole, p2)
      vect(:,i) += hij * coefs
    end do
    do i=hole+1,mo_tot_num
      if(lbanned(i)) cycle
      hij = (mo_bielec_integral(p1, p2, hole, i) - mo_bielec_integral(p2, p1, hole, i))
      hij *= get_phase_bi(phasemask, sp, sp, hole, p1, i, p2)
      vect(:,i) += hij * coefs
    end do

    call apply_particle(mask, sp, p2, det, ok,  N_int)
    call i_h_j(gen, det, N_int, hij)
    vect(:, p2) += hij * coefs
  else
    p2 = p(1, sh)
    do i=1,mo_tot_num
      if(lbanned(i)) cycle
      hij = mo_bielec_integral(p1, p2, i, hole)
      hij *= get_phase_bi(phasemask, sp, sh, i, p1, hole, p2)
      vect(:,i) += hij * coefs
    end do
  end if

  call apply_particle(mask, sp, p1, det, ok,  N_int)
  call i_h_j(gen, det, N_int, hij)
  vect(:, p1) += hij * coefs
end subroutine


subroutine get_m0(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: gen(N_int, 2), mask(N_int, 2)
  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
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

  do i=1, N
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
  end do 
end subroutine




! Selection double
! ----------------
 
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
  integer                         :: h1,h2,s1,s2,s3,i1,i2,ib,sp,k,i,j,nt,ii
  integer(bit_kind)               :: hole(N_int,2), particle(N_int,2), mask(N_int, 2), pmask(N_int, 2)
  logical                         :: fullMatch, ok
  
  integer(bit_kind) :: mobMask(N_int, 2), negMask(N_int, 2)
  integer,allocatable               :: preinteresting(:), prefullinteresting(:), interesting(:), fullinteresting(:)
  integer(bit_kind), allocatable :: minilist(:, :, :), fullminilist(:, :, :)
  
  allocate(minilist(N_int, 2, N_det_selectors), fullminilist(N_int, 2, N_det))
  allocate(preinteresting(0:N_det_selectors), prefullinteresting(0:N_det), interesting(0:N_det_selectors), fullinteresting(0:N_det))
  
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

  
  preinteresting(0) = 0
  prefullinteresting(0) = 0
  
  do i=1,N_int
    negMask(i,1) = not(psi_det_generators(i,1,i_generator))
    negMask(i,2) = not(psi_det_generators(i,2,i_generator))
  end do
  
  do i=1,N_det
    nt = 0
    do j=1,N_int
      mobMask(j,1) = iand(negMask(j,1), psi_selectors(j,1,i))
      mobMask(j,2) = iand(negMask(j,2), psi_selectors(j,2,i))
      nt += popcnt(mobMask(j, 1)) + popcnt(mobMask(j, 2))
    end do

    if(nt <= 4) then
      if(i <= N_det_selectors) then
        preinteresting(0) += 1
        preinteresting(preinteresting(0)) = i
      else if(nt <= 2) then
        prefullinteresting(0) += 1
        prefullinteresting(prefullinteresting(0)) = i
      end if
    end if
  end do
  
        
  do s1=1,2
    do i1=N_holes(s1),1,-1   ! Generate low excitations first
      h1 = hole_list(i1,s1)
      call apply_hole(psi_det_generators(1,1,i_generator), s1,h1, pmask, ok, N_int)
      
      do i=1,N_int
        negMask(i,1) = not(pmask(i,1))
        negMask(i,2) = not(pmask(i,2))
      end do
      
      interesting(0) = 0
      fullinteresting(0) = 0
      
      do ii=1,preinteresting(0)
        i = preinteresting(ii)
        nt = 0
        do j=1,N_int
          mobMask(j,1) = iand(negMask(j,1), psi_selectors(j,1,i))
          mobMask(j,2) = iand(negMask(j,2), psi_selectors(j,2,i))
          nt += popcnt(mobMask(j, 1)) + popcnt(mobMask(j, 2))
        end do
        
        if(nt <= 4) then
          interesting(0) += 1
          interesting(interesting(0)) = i
          minilist(:,:,interesting(0)) = psi_selectors(:,:,i)
          if(nt <= 2) then
            fullinteresting(0) += 1
            fullinteresting(fullinteresting(0)) = i
            fullminilist(:,:,fullinteresting(0)) = psi_selectors(:,:,i)
          end if
        end if
      end do
      
      do ii=1,prefullinteresting(0)
        i = prefullinteresting(ii)
        nt = 0
        do j=1,N_int
          mobMask(j,1) = iand(negMask(j,1), psi_selectors(j,1,i))
          mobMask(j,2) = iand(negMask(j,2), psi_selectors(j,2,i))
          nt += popcnt(mobMask(j, 1)) + popcnt(mobMask(j, 2))
        end do

        if(nt <= 2) then
          fullinteresting(0) += 1
          fullinteresting(fullinteresting(0)) = i
          fullminilist(:,:,fullinteresting(0)) = psi_selectors(:,:,i)
        end if
      end do
      
      do s2=s1,2
        sp = s1
        if(s1 /= s2) sp = 3
        
        ib = 1
        if(s1 == s2) ib = i1+1
        do i2=N_holes(s2),ib,-1   ! Generate low excitations first
          
          h2 = hole_list(i2,s2)
          call apply_hole(pmask, s2,h2, mask, ok, N_int)
          
          logical                        :: banned(mo_tot_num, mo_tot_num,2)
          logical                        :: bannedOrb(mo_tot_num, 2)
          
          banned = .false.
          
          call spot_isinwf(mask, fullminilist, i_generator, fullinteresting(0), banned, fullMatch, fullinteresting)
          
          if(fullMatch) cycle
          
          bannedOrb(1:mo_tot_num, 1:2) = .true.
          do s3=1,2
            do i=1,N_particles(s3)
              bannedOrb(particle_list(i,s3), s3) = .false.
            enddo
          enddo
             
          mat = 0d0
          call splash_pq(mask, sp, minilist, i_generator, interesting(0), bannedOrb, banned, mat, interesting)
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
  
  integer, intent(in)            :: i_generator, sp, h1, h2
  double precision, intent(in)   :: mat(N_states, mo_tot_num, mo_tot_num)
  logical, intent(in)            :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num)
  double precision, intent(in)   :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)   :: E0(N_states)
  double precision, intent(inout) :: pt2(N_states)
  type(selection_buffer), intent(inout) :: buf
  logical                        :: ok
  integer                        :: s1, s2, p1, p2, ib, j, istate
  integer(bit_kind)              :: mask(N_int, 2), det(N_int, 2)
  double precision               :: e_pert, delta_E, val, Hii, max_e_pert,tmp
  double precision, external     :: diag_H_mat_elem_fock
  
  logical, external              :: detEq
  
  
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
      logical, external              :: is_in_wavefunction
      
      if (do_ddci) then
        logical, external              :: is_a_two_holes_two_particles
        if (is_a_two_holes_two_particles(det)) then
          cycle
        endif
      endif
      
      Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),det,fock_diag_tmp,N_int)
      max_e_pert = 0d0
      
      do istate=1,N_states
        delta_E = E0(istate) - Hii
        val = mat(istate, p1, p2) + mat(istate, p1, p2)
        tmp = dsqrt(delta_E * delta_E + val * val)
        if (delta_E < 0.d0) then
          tmp = -tmp
        endif
        e_pert = 0.5d0 * ( tmp - delta_E)
        pt2(istate) = pt2(istate) + e_pert
        max_e_pert = min(e_pert,max_e_pert)
      end do
      
      if(dabs(max_e_pert) > buf%mini) then
        call add_to_selection_buffer(buf, det, max_e_pert)
      end if
    end do
  end do
end subroutine


subroutine splash_pq(mask, sp, det, i_gen, N_sel, bannedOrb, banned, mat, interesting)
  use bitmasks
  implicit none
  
  integer, intent(in) :: interesting(0:N_sel)
  
  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int, 2, N_sel)
  integer, intent(in) :: sp, i_gen, N_sel
  logical, intent(inout) :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num, 2)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)

  integer :: i, ii, j, k, l, h(0:2,2), p(0:4,2), nt
  integer(bit_kind) :: perMask(N_int, 2), mobMask(N_int, 2), negMask(N_int, 2)
!   logical :: bandon
!   
!   bandon = .false.
  mat = 0d0
  
  do i=1,N_int
    negMask(i,1) = not(mask(i,1))
    negMask(i,2) = not(mask(i,2))
  end do

  do i=1, N_sel ! interesting(0)
    !i = interesting(ii)
    
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
    
    if(interesting(i) < i_gen) then
      if(nt == 4) call past_d2(banned, p, sp)
      if(nt == 3) call past_d1(bannedOrb, p)
    else
      if(interesting(i) == i_gen) then
!         bandon = .true.
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
        call get_d2(det(1,1,i), psi_phasemask(1,1,interesting(i)), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, interesting(i)))
      else if(nt == 3) then
        call get_d1(det(1,1,i), psi_phasemask(1,1,interesting(i)), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, interesting(i)))
      else
        call get_d0(det(1,1,i), psi_phasemask(1,1,interesting(i)), bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, interesting(i)))
      end if
    end if
  end do
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
  
  double precision, external :: get_phase_bi, mo_bielec_integral
  
  integer :: i, j, tip, ma, mi, puti, putj
  integer :: h1, h2, p1, p2, i1, i2
  double precision :: hij, phase
  
  integer, parameter:: turn2d(2,3,4) = reshape((/0,0, 0,0, 0,0,  3,4, 0,0, 0,0,  2,4, 1,4, 0,0,  2,3, 1,3, 1,2 /), (/2,3,4/))
  integer, parameter :: turn2(2) = (/2, 1/)
  integer, parameter :: turn3(2,3) = reshape((/2,3,  1,3, 1,2/), (/2,3/))
  
  integer :: bant
  bant = 1

  tip = p(0,1) * p(0,2)
  
  ma = sp
  if(p(0,1) > p(0,2)) ma = 1
  if(p(0,1) < p(0,2)) ma = 2
  mi = mod(ma, 2) + 1
  
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
        
        hij = (mo_bielec_integral(p1, p2, h1, h2) - mo_bielec_integral(p2,p1, h1, h2)) * get_phase_bi(phasemask, ma, ma, h1, p1, h2, p2)
        if(ma == 1) then
          mat(:, putj, puti) += coefs * hij
        else
          mat(:, puti, putj) += coefs * hij
        end if
      end do
    else
      do i = 1,2
      do j = 1,2
        puti = p(i, 1)
        putj = p(j, 2)
        
        if(banned(puti,putj,bant)) cycle
        p1 = p(turn2(i), 1)
        p2 = p(turn2(j), 2)
        h1 = h(1,1)
        h2 = h(1,2)
        
        hij = mo_bielec_integral(p1, p2, h1, h2) * get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
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
        hij = (mo_bielec_integral(p1, p2, h1, h2) - mo_bielec_integral(p2,p1, h1, h2)) * get_phase_bi(phasemask, ma, ma, h1, p1, h2, p2)
        mat(:, puti, putj) += coefs * hij
      end do
      end do
    else if(tip == 3) then
      h1 = h(1, mi)
      h2 = h(1, ma)
      p1 = p(1, mi)
      do i=1,3
        puti = p(turn3(1,i), ma)
        putj = p(turn3(2,i), ma)
        if(banned(puti,putj,1)) cycle
        p2 = p(i, ma)
        
        hij = mo_bielec_integral(p1, p2, h1, h2) * get_phase_bi(phasemask, mi, ma, h1, p1, h2, p2)
        mat(:, min(puti, putj), max(puti, putj)) += coefs * hij
      end do
    else ! tip == 4
      puti = p(1, sp)
      putj = p(2, sp)
      if(.not. banned(puti,putj,1)) then
        p1 = p(1, mi)
        p2 = p(2, mi)
        h1 = h(1, mi)
        h2 = h(2, mi)
        hij = (mo_bielec_integral(p1, p2, h1, h2) - mo_bielec_integral(p2,p1, h1, h2)) * get_phase_bi(phasemask, mi, mi, h1, p1, h2, p2)
        mat(:, puti, putj) += coefs * hij
      end if
    end if
  end if
end subroutine


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
  double precision, external :: get_phase_bi, mo_bielec_integral
  
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

  if(sp == 3) then
    !move MA
    if(ma == 2) bant = 2
    puti = p(1,mi)
    hfix = h(1,ma)
    p1 = p(1,ma)
    p2 = p(2,ma)
    if(.not. bannedOrb(puti, mi)) then
      tmp_row = 0d0
      do putj=1, hfix-1
        if(lbanned(putj, ma) .or. banned(putj, puti,bant)) cycle
        hij = (mo_bielec_integral(p1, p2, putj, hfix)-mo_bielec_integral(p2,p1,putj,hfix)) * get_phase_bi(phasemask, ma, ma, putj, p1, hfix, p2)
        tmp_row(1:N_states,putj) += hij * coefs(1:N_states)
      end do
      do putj=hfix+1, mo_tot_num
        if(lbanned(putj, ma) .or. banned(putj, puti,bant)) cycle
        hij = (mo_bielec_integral(p1, p2, hfix, putj)-mo_bielec_integral(p2,p1,hfix,putj)) * get_phase_bi(phasemask, ma, ma, hfix, p1, putj, p2)
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
        hij = mo_bielec_integral(p2,pfix,hfix,puti) * get_phase_bi(phasemask, ma, mi, hfix, p2, puti, pfix)
        tmp_row(:,puti) += hij * coefs
      end if
      
      putj = p2
      if(.not. banned(putj,puti,bant)) then
        hij = mo_bielec_integral(p1,pfix,hfix,puti) * get_phase_bi(phasemask, ma, mi, hfix, p1, puti, pfix)
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
          hij = (mo_bielec_integral(p1, p2, putj, hfix)-mo_bielec_integral(p2,p1,putj,hfix)) * get_phase_bi(phasemask, ma, ma, putj, p1, hfix, p2)
          tmp_row(:,putj) += hij * coefs
        end do
        do putj=hfix+1,mo_tot_num
          if(lbanned(putj,ma) .or. banned(puti,putj,1)) cycle
          hij = (mo_bielec_integral(p1, p2, hfix, putj)-mo_bielec_integral(p2,p1,hfix,putj)) * get_phase_bi(phasemask, ma, ma, hfix, p1, putj, p2)
          tmp_row(:,putj) += hij * coefs
        end do

        mat(:, :puti-1, puti) += tmp_row(:,:puti-1)
        mat(:, puti, puti:) += tmp_row(:,puti:)
      end do
    else
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
          hij = mo_bielec_integral(pfix, p1, hfix, puti) * get_phase_bi(phasemask, mi, ma, hfix, pfix, puti, p1)
          tmp_row(:,puti) += hij * coefs
        end if
        
        putj = p1
        if(.not. banned(puti,putj,1)) then
          hij = mo_bielec_integral(pfix, p2, hfix, puti) * get_phase_bi(phasemask, mi, ma, hfix, pfix, puti, p2)
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
  double precision, external :: get_phase_bi, mo_bielec_integral
  logical :: ok
  
  integer :: bant
  bant = 1
  

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
          call i_h_j(gen, det, N_int, hij)
        else
          hij = mo_bielec_integral(p1, p2, h1, h2) * get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
          phase = get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
        end if
        mat(:, p1, p2) += coefs(:) * hij
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
        else
          hij = (mo_bielec_integral(p1, p2, puti, putj) -  mo_bielec_integral(p2, p1, puti, putj))* get_phase_bi(phasemask, sp, sp, puti, p1 , putj, p2)
        end if
        mat(:, puti, putj) += coefs(:) * hij
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



subroutine spot_isinwf(mask, det, i_gen, N, banned, fullMatch, interesting)
  use bitmasks
  implicit none
  
  integer, intent(in) :: interesting(0:N)
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

    if(interesting(i) < i_gen) then
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


subroutine ZMQ_selection(N_in, pt2)
  use f77_zmq
  use selection_types
  
  implicit none
  
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
  integer, intent(in)            :: N_in
  type(selection_buffer)         :: b
  integer                        :: i, N
  integer, external              :: omp_get_thread_num
  double precision, intent(out)  :: pt2(N_states)
  integer, parameter             :: maxtasks=10000
  
  
  N = max(N_in,1)
  if (.True.) then
    PROVIDE pt2_e0_denominator
    provide nproc
    call new_parallel_job(zmq_to_qp_run_socket,"selection")
    call zmq_put_psi(zmq_to_qp_run_socket,1,pt2_e0_denominator,size(pt2_e0_denominator))
    call create_selection_buffer(N, N*2, b)
  endif

  character*(20*maxtasks) :: task
  task = ' '

  integer :: k
  k=0
  do i= 1, N_det_generators
    k = k+1
    write(task(20*(k-1)+1:20*k),'(I9,1X,I9,''|'')') i, N
    if (k>=maxtasks) then
       k=0
       call add_task_to_taskserver(zmq_to_qp_run_socket,task)
    endif
  enddo
  if (k > 0) then
    call add_task_to_taskserver(zmq_to_qp_run_socket,task)
  endif
  call zmq_set_running(zmq_to_qp_run_socket)

  !$OMP PARALLEL DEFAULT(shared)  SHARED(b, pt2)  PRIVATE(i) NUM_THREADS(nproc+1)
  i = omp_get_thread_num()
  if (i==0) then
    call selection_collector(b, pt2)
  else
    call selection_slave_inproc(i)
  endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, 'selection')
  if (N_in > 0) then
    call fill_H_apply_buffer_no_selection(b%cur,b%det,N_int,0) !!! PAS DE ROBIN
    call copy_H_apply_buffer_to_wf()
    if (s2_eig) then
      call make_s2_eigenfunction
    endif
    call save_wavefunction
  endif
end subroutine


subroutine selection_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call run_selection_slave(1,i,pt2_e0_denominator)
end

subroutine selection_collector(b, pt2)
  use f77_zmq
  use selection_types
  use bitmasks
  implicit none


  type(selection_buffer), intent(inout) :: b
  double precision, intent(out)       :: pt2(N_states)
  double precision                   :: pt2_mwen(N_states)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull

  integer :: msg_size, rc, more
  integer :: acc, i, j, robin, N, ntask
  double precision, allocatable :: val(:)
  integer(bit_kind), allocatable :: det(:,:,:)
  integer, allocatable :: task_id(:)
  integer :: done
  real :: time, time0
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  allocate(val(b%N), det(N_int, 2, b%N), task_id(N_det))
  done = 0
  more = 1
  pt2(:) = 0d0
  call CPU_TIME(time0)
  do while (more == 1)
    call pull_selection_results(zmq_socket_pull, pt2_mwen, val(1), det(1,1,1), N, task_id, ntask)
    pt2 += pt2_mwen
    do i=1, N
      call add_to_selection_buffer(b, det(1,1,i), val(i))
    end do

    do i=1, ntask
      if(task_id(i) == 0) then
          print *,  "Error in collector"
      endif
      call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id(i),more)
    end do
    done += ntask
    call CPU_TIME(time)
!    print *, "DONE" , done, time - time0
  end do


  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_pull_socket(zmq_socket_pull)
  call sort_selection_buffer(b)
end subroutine

