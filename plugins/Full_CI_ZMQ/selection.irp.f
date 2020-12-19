use bitmasks

BEGIN_PROVIDER [ integer, fragment_count ]
  implicit none
  BEGIN_DOC
  ! Number of fragments for the deterministic part
  END_DOC
  fragment_count = (elec_alpha_num-n_core_orb)**2
END_PROVIDER


subroutine assert(cond, msg)
  character(*), intent(in) :: msg
  logical, intent(in) :: cond
  
  if(.not. cond) then
    print *, "assert failed: "//msg
    stop
  end if
end subroutine


subroutine get_mask_phase(det, phasemask)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in)  :: det(N_int, 2)
  integer, intent(out)        :: phasemask(2,N_int*bit_kind_size)
  integer                        :: s, ni, i
  logical                        :: change
  
  phasemask = 0_1
  do s=1,2
    change = .false.
    do ni=1,N_int
      do i=0,bit_kind_size-1
        if(BTEST(det(ni, s), i)) then
          change = .not. change
        endif
        if(change) then
          phasemask(s, ishft(ni-1,bit_kind_shift) + i + 1) = 1_1
        endif
      end do
    end do
  end do
end subroutine


subroutine select_connected(i_generator,E0,pt2,b,subset)
  use bitmasks
  use selection_types
  implicit none
  integer, intent(in)            :: i_generator, subset
  type(selection_buffer), intent(inout) :: b
  double precision, intent(inout)  :: pt2(N_states)
  integer :: k,l
  double precision, intent(in)   :: E0(N_states)

  integer(bit_kind)              :: hole_mask(N_int,2), particle_mask(N_int,2)

  double precision, allocatable  :: fock_diag_tmp(:,:)

  allocate(fock_diag_tmp(2,mo_tot_num+1))
  
  call build_fock_tmp(fock_diag_tmp,psi_det_generators(1,1,i_generator),N_int)

  do l=1,N_generators_bitmask
    do k=1,N_int
      hole_mask(k,1) = iand(generators_bitmask(k,1,s_hole,l), psi_det_generators(k,1,i_generator))
      hole_mask(k,2) = iand(generators_bitmask(k,2,s_hole,l), psi_det_generators(k,2,i_generator))
      particle_mask(k,1) = iand(generators_bitmask(k,1,s_part,l), not(psi_det_generators(k,1,i_generator)) )
      particle_mask(k,2) = iand(generators_bitmask(k,2,s_part,l), not(psi_det_generators(k,2,i_generator)) )
    enddo
    call select_singles_and_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b,subset)
  enddo
  deallocate(fock_diag_tmp)
end subroutine


double precision function get_phase_bi(phasemask, s1, s2, h1, p1, h2, p2)
  use bitmasks
  implicit none

  integer, intent(in) :: phasemask(2,*)
  integer, intent(in) :: s1, s2, h1, h2, p1, p2
  logical :: change
  integer :: np1
  integer :: np
  double precision, save :: res(0:1) = (/1d0, -1d0/)

  np1 = phasemask(s1,h1) + phasemask(s1,p1) + phasemask(s2,h2) + phasemask(s2,p2)
  np = np1
  if(p1 < h1) np = np + 1
  if(p2 < h2) np = np + 1
  
  if(s1 == s2 .and. max(h1, p1) > min(h2, p2)) np = np + 1
  get_phase_bi = res(iand(np,1))
end 



subroutine get_m2(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: gen(N_int, 2), mask(N_int, 2)
  integer, intent(in) :: phasemask(2,N_int*bit_kind_size)
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
end 



subroutine get_m1(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in)  :: gen(N_int, 2), mask(N_int, 2)
  integer, intent(in)         :: phasemask(2,N_int*bit_kind_size)
  logical, intent(in)            :: bannedOrb(mo_tot_num)
  double precision, intent(in)   :: coefs(N_states)
  double precision, intent(inout) :: vect(N_states, mo_tot_num)
  integer, intent(in)            :: sp, h(0:2, 2), p(0:3, 2)
  integer                        :: i, hole, p1, p2, sh
  logical                        :: ok

  logical, allocatable           :: lbanned(:)
  integer(bit_kind)              :: det(N_int, 2)
  double precision               :: hij
  double precision, external     :: get_phase_bi, mo_bielec_integral
  
  allocate (lbanned(mo_tot_num))
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
      vect(1:N_states,i) += hij * coefs(1:N_states)
    end do
    do i=hole+1,mo_tot_num
      if(lbanned(i)) cycle
      hij = (mo_bielec_integral(p1, p2, hole, i) - mo_bielec_integral(p2, p1, hole, i))
      hij *= get_phase_bi(phasemask, sp, sp, hole, p1, i, p2)
      vect(1:N_states,i) += hij * coefs(1:N_states)
    end do

    call apply_particle(mask, sp, p2, det, ok,  N_int)
    call i_h_j(gen, det, N_int, hij)
    vect(1:N_states, p2) += hij * coefs(1:N_states)
  else
    p2 = p(1, sh)
    do i=1,mo_tot_num
      if(lbanned(i)) cycle
      hij = mo_bielec_integral(p1, p2, i, hole)
      hij *= get_phase_bi(phasemask, sp, sh, i, p1, hole, p2)
      vect(1:N_states,i) += hij * coefs(1:N_states)
    end do
  end if
  deallocate(lbanned)

  call apply_particle(mask, sp, p1, det, ok,  N_int)
  call i_h_j(gen, det, N_int, hij)
  vect(1:N_states, p1) += hij * coefs(1:N_states)
end 


subroutine get_m0(gen, phasemask, bannedOrb, vect, mask, h, p, sp, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in)  :: gen(N_int, 2), mask(N_int, 2)
  integer, intent(in)         :: phasemask(2,N_int*bit_kind_size)
  logical, intent(in)            :: bannedOrb(mo_tot_num)
  double precision, intent(in)   :: coefs(N_states)
  double precision, intent(inout) :: vect(N_states, mo_tot_num)
  integer, intent(in)            :: sp, h(0:2, 2), p(0:3, 2)
  integer                        :: i
  logical                        :: ok

  logical, allocatable           :: lbanned(:)
  integer(bit_kind)              :: det(N_int, 2)
  double precision               :: hij
  
  allocate(lbanned(mo_tot_num))
  lbanned = bannedOrb
  lbanned(p(1,sp)) = .true.
  do i=1,mo_tot_num
    if(lbanned(i)) cycle
    call apply_particle(mask, sp, i, det, ok, N_int)
    call i_h_j(gen, det, N_int, hij)
    vect(1:N_states, i) += hij * coefs(1:N_states)
  end do
  deallocate(lbanned)
end 


subroutine select_singles_and_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,buf,subset)
  use bitmasks
  use selection_types
  implicit none
  BEGIN_DOC
!            WARNING /!\ : It is assumed that the generators and selectors are psi_det_sorted
  END_DOC
  
  integer, intent(in)            :: i_generator, subset
  integer(bit_kind), intent(in)  :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)   :: fock_diag_tmp(mo_tot_num)
  double precision, intent(in)   :: E0(N_states)
  double precision, intent(inout) :: pt2(N_states)
  type(selection_buffer), intent(inout) :: buf
  
  integer                         :: h1,h2,s1,s2,s3,i1,i2,ib,sp,k,i,j,nt,ii
  integer(bit_kind)               :: hole(N_int,2), particle(N_int,2), mask(N_int, 2), pmask(N_int, 2)
  logical                         :: fullMatch, ok
  
  integer(bit_kind) :: mobMask(N_int, 2), negMask(N_int, 2)
  integer,allocatable               :: preinteresting(:), prefullinteresting(:), interesting(:), fullinteresting(:)
  integer(bit_kind), allocatable :: minilist(:, :, :), fullminilist(:, :, :)
  logical, allocatable           :: banned(:,:,:), bannedOrb(:,:)

  double precision, allocatable   :: mat(:,:,:)
  
  logical :: monoAdo, monoBdo
  integer :: maskInd

  integer(bit_kind), allocatable:: preinteresting_det(:,:,:)
  allocate (preinteresting_det(N_int,2,N_det))

  PROVIDE fragment_count

  monoAdo = .true.
  monoBdo = .true.

  
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

  integer :: l_a, nmax
  integer, allocatable :: indices(:), exc_degree(:), iorder(:)
  allocate (indices(N_det),  &
            exc_degree(max(N_det_alpha_unique,N_det_beta_unique)))

  PROVIDE psi_bilinear_matrix_columns_loc psi_det_alpha_unique psi_det_beta_unique
  PROVIDE psi_bilinear_matrix_rows psi_det_sorted_order psi_bilinear_matrix_order
  PROVIDE psi_bilinear_matrix_transp_rows_loc psi_bilinear_matrix_transp_columns
  PROVIDE psi_bilinear_matrix_transp_order

  k=1
  do i=1,N_det_alpha_unique
    call get_excitation_degree_spin(psi_det_alpha_unique(1,i), &
      psi_det_generators(1,1,i_generator), exc_degree(i), N_int)
  enddo

  do j=1,N_det_beta_unique
    call get_excitation_degree_spin(psi_det_beta_unique(1,j), &
      psi_det_generators(1,2,i_generator), nt, N_int)
    if (nt > 2) cycle
    do l_a=psi_bilinear_matrix_columns_loc(j), psi_bilinear_matrix_columns_loc(j+1)-1
      i = psi_bilinear_matrix_rows(l_a)
      if (nt + exc_degree(i) <= 4) then
        indices(k) = psi_det_sorted_order(psi_bilinear_matrix_order(l_a))
        k=k+1
      endif
    enddo
  enddo
  
  do i=1,N_det_beta_unique
    call get_excitation_degree_spin(psi_det_beta_unique(1,i), &
      psi_det_generators(1,2,i_generator), exc_degree(i), N_int)
  enddo

  do j=1,N_det_alpha_unique
    call get_excitation_degree_spin(psi_det_alpha_unique(1,j), &
      psi_det_generators(1,1,i_generator), nt, N_int)
    if (nt > 1) cycle
    do l_a=psi_bilinear_matrix_transp_rows_loc(j), psi_bilinear_matrix_transp_rows_loc(j+1)-1
      i = psi_bilinear_matrix_transp_columns(l_a)
      if (exc_degree(i) < 3) cycle
      if (nt + exc_degree(i) <= 4) then
        indices(k) = psi_det_sorted_order(                   &
                        psi_bilinear_matrix_order(           &
                          psi_bilinear_matrix_transp_order(l_a)))
        k=k+1
      endif
    enddo
  enddo
  deallocate(exc_degree)
  nmax=k-1

  allocate(iorder(nmax))
  do i=1,nmax
    iorder(i) = i
  enddo
  call isort(indices,iorder,nmax)
  deallocate(iorder)

  allocate(preinteresting(0:N_det_selectors), prefullinteresting(0:N_det), &
            interesting(0:N_det_selectors), fullinteresting(0:N_det))
  preinteresting(0) = 0
  prefullinteresting(0) = 0
  
  do i=1,N_int
    negMask(i,1) = not(psi_det_generators(i,1,i_generator))
    negMask(i,2) = not(psi_det_generators(i,2,i_generator))
  end do
  
  do k=1,nmax
    i = indices(k)
    mobMask(1,1) = iand(negMask(1,1), psi_det_sorted(1,1,i))
    mobMask(1,2) = iand(negMask(1,2), psi_det_sorted(1,2,i))
    nt = popcnt(mobMask(1, 1)) + popcnt(mobMask(1, 2)) 
    do j=2,N_int
      mobMask(j,1) = iand(negMask(j,1), psi_det_sorted(j,1,i))
      mobMask(j,2) = iand(negMask(j,2), psi_det_sorted(j,2,i))
      nt = nt + popcnt(mobMask(j, 1)) + popcnt(mobMask(j, 2)) 
    end do

    if(nt <= 4) then
      if(i <= N_det_selectors) then
        preinteresting(0) += 1
        preinteresting(preinteresting(0)) = i
        do j=1,N_int
          preinteresting_det(j,1,preinteresting(0)) = psi_det_sorted(j,1,i)
          preinteresting_det(j,2,preinteresting(0)) = psi_det_sorted(j,2,i)
        enddo
      else if(nt <= 2) then
        prefullinteresting(0) += 1
        prefullinteresting(prefullinteresting(0)) = i
      end if
    end if
  end do
  deallocate(indices)
  

  allocate(minilist(N_int, 2, N_det_selectors), fullminilist(N_int, 2, N_det))
  allocate(banned(mo_tot_num, mo_tot_num,2), bannedOrb(mo_tot_num, 2))
  allocate (mat(N_states, mo_tot_num, mo_tot_num))
  maskInd = -1
  integer :: nb_count
  do s1=1,2
    do i1=N_holes(s1),1,-1   ! Generate low excitations first
      h1 = hole_list(i1,s1)
      call apply_hole(psi_det_generators(1,1,i_generator), s1,h1, pmask, ok, N_int)
      
      negMask = not(pmask)
      
      interesting(0) = 0
      fullinteresting(0) = 0
      
      do ii=1,preinteresting(0)
        select case (N_int)
          case (1)
            mobMask(1,1) = iand(negMask(1,1), preinteresting_det(1,1,ii))
            mobMask(1,2) = iand(negMask(1,2), preinteresting_det(1,2,ii))
            nt = popcnt(mobMask(1, 1)) + popcnt(mobMask(1, 2))
          case (2)
            mobMask(1:2,1) = iand(negMask(1:2,1), preinteresting_det(1:2,1,ii))
            mobMask(1:2,2) = iand(negMask(1:2,2), preinteresting_det(1:2,2,ii))
            nt = popcnt(mobMask(1, 1)) + popcnt(mobMask(1, 2)) + &
                 popcnt(mobMask(2, 1)) + popcnt(mobMask(2, 2)) 
          case (3)
            mobMask(1:3,1) = iand(negMask(1:3,1), preinteresting_det(1:3,1,ii))
            mobMask(1:3,2) = iand(negMask(1:3,2), preinteresting_det(1:3,2,ii))
            nt = 0
            do j=3,1,-1
              if (mobMask(j,1) /= 0_bit_kind) then
                nt = nt+ popcnt(mobMask(j, 1))
                if (nt > 4) exit
              endif
              if (mobMask(j,2) /= 0_bit_kind) then
                nt = nt+ popcnt(mobMask(j, 2))
                if (nt > 4) exit
              endif
            end do
          case (4)
            mobMask(1:4,1) = iand(negMask(1:4,1), preinteresting_det(1:4,1,ii))
            mobMask(1:4,2) = iand(negMask(1:4,2), preinteresting_det(1:4,2,ii))
            nt = 0
            do j=4,1,-1
              if (mobMask(j,1) /= 0_bit_kind) then
                nt = nt+ popcnt(mobMask(j, 1))
                if (nt > 4) exit
              endif
              if (mobMask(j,2) /= 0_bit_kind) then
                nt = nt+ popcnt(mobMask(j, 2))
                if (nt > 4) exit
              endif
            end do
          case default
            mobMask(1:N_int,1) = iand(negMask(1:N_int,1), preinteresting_det(1:N_int,1,ii))
            mobMask(1:N_int,2) = iand(negMask(1:N_int,2), preinteresting_det(1:N_int,2,ii))
            nt = 0 
            do j=N_int,1,-1
              if (mobMask(j,1) /= 0_bit_kind) then
                nt = nt+ popcnt(mobMask(j, 1))
                if (nt > 4) exit
              endif
              if (mobMask(j,2) /= 0_bit_kind) then
                nt = nt+ popcnt(mobMask(j, 2))
                if (nt > 4) exit
              endif
            end do
        end select
        
        if(nt <= 4) then
          i = preinteresting(ii)
          interesting(0) += 1
          interesting(interesting(0)) = i
          minilist(1,1,interesting(0)) = preinteresting_det(1,1,ii)
          minilist(1,2,interesting(0)) = preinteresting_det(1,2,ii)
          do j=2,N_int
            minilist(j,1,interesting(0)) = preinteresting_det(j,1,ii)
            minilist(j,2,interesting(0)) = preinteresting_det(j,2,ii)
          enddo
          if(nt <= 2) then
            fullinteresting(0) += 1
            fullinteresting(fullinteresting(0)) = i
            fullminilist(1,1,fullinteresting(0)) = preinteresting_det(1,1,ii)
            fullminilist(1,2,fullinteresting(0)) = preinteresting_det(1,2,ii)
            do j=2,N_int
              fullminilist(j,1,fullinteresting(0)) = preinteresting_det(j,1,ii)
              fullminilist(j,2,fullinteresting(0)) = preinteresting_det(j,2,ii)
            enddo
          end if
        end if
        
      end do
      
      do ii=1,prefullinteresting(0)
        i = prefullinteresting(ii)
        nt = 0
        mobMask(1,1) = iand(negMask(1,1), psi_det_sorted(1,1,i))
        mobMask(1,2) = iand(negMask(1,2), psi_det_sorted(1,2,i))
        nt = popcnt(mobMask(1, 1)) + popcnt(mobMask(1, 2))
        if (nt > 2) cycle
        do j=N_int,2,-1
          mobMask(j,1) = iand(negMask(j,1), psi_det_sorted(j,1,i))
          mobMask(j,2) = iand(negMask(j,2), psi_det_sorted(j,2,i))
          nt = nt+ popcnt(mobMask(j, 1)) + popcnt(mobMask(j, 2))
          if (nt > 2) exit
        end do
        
        if(nt <= 2) then
          fullinteresting(0) += 1
          fullinteresting(fullinteresting(0)) = i
          fullminilist(1,1,fullinteresting(0)) = psi_det_sorted(1,1,i)
          fullminilist(1,2,fullinteresting(0)) = psi_det_sorted(1,2,i)
          do j=2,N_int
            fullminilist(j,1,fullinteresting(0)) = psi_det_sorted(j,1,i)
            fullminilist(j,2,fullinteresting(0)) = psi_det_sorted(j,2,i)
          enddo
        end if
      end do
      


      do s2=s1,2
        sp = s1
        
        if(s1 /= s2) sp = 3
        
        ib = 1
        if(s1 == s2) ib = i1+1
        monoAdo = .true.
        do i2=N_holes(s2),ib,-1   ! Generate low excitations first
          
          h2 = hole_list(i2,s2)
          call apply_hole(pmask, s2,h2, mask, ok, N_int)
          banned = .false.
          do j=1,mo_tot_num
            bannedOrb(j, 1) = .true.
            bannedOrb(j, 2) = .true.
          enddo
          do s3=1,2
            do i=1,N_particles(s3)
              bannedOrb(particle_list(i,s3), s3) = .false.
            enddo
          enddo
          if(s1 /= s2) then
            if(monoBdo) then
              bannedOrb(h1,s1) = .false.
            end if
            if(monoAdo) then
              bannedOrb(h2,s2) = .false.
              monoAdo = .false.
            end if
          end if

          maskInd += 1
          if(subset == 0 .or. mod(maskInd, fragment_count) == (subset-1)) then  
            
            call spot_isinwf(mask, fullminilist, i_generator, fullinteresting(0), banned, fullMatch, fullinteresting)
            if(fullMatch) cycle
          
            call splash_pq(mask, sp, minilist, i_generator, interesting(0), bannedOrb, banned, mat, interesting)

            call fill_buffer_double(i_generator, sp, h1, h2, bannedOrb, banned, fock_diag_tmp, E0, pt2, mat, buf)
          end if
        enddo
        if(s1 /= s2) monoBdo = .false.
      enddo
    enddo
  enddo
  deallocate(preinteresting, prefullinteresting, interesting, fullinteresting)
  deallocate(minilist, fullminilist, banned, bannedOrb,mat)
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
  double precision :: e_pert, delta_E, val, Hii, min_e_pert,tmp
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

      if( sum(abs(mat(1:N_states, p1, p2))) == 0d0) cycle
      call apply_particles(mask, s1, p1, s2, p2, det, ok, N_int)
      
      Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),det,fock_diag_tmp,N_int)
      min_e_pert = 0d0
      
      do istate=1,N_states
        delta_E = E0(istate) - Hii
        val = mat(istate, p1, p2) + mat(istate, p1, p2) 
        tmp = dsqrt(delta_E * delta_E + val * val)
        if (delta_E < 0.d0) then
            tmp = -tmp
        endif
        e_pert = 0.5d0 * (tmp - delta_E)
        pt2(istate) = pt2(istate) + e_pert
        min_e_pert = min(e_pert,min_e_pert)
      end do
      
      if(min_e_pert <= buf%mini) then
        call add_to_selection_buffer(buf, det, min_e_pert)
      end if
    end do
  end do
end 


subroutine splash_pq(mask, sp, det, i_gen, N_sel, bannedOrb, banned, mat, interesting)
  use bitmasks
  implicit none
  
  integer, intent(in)            :: sp, i_gen, N_sel
  integer, intent(in)            :: interesting(0:N_sel)
  integer(bit_kind),intent(in)   :: mask(N_int, 2), det(N_int, 2, N_sel)
  logical, intent(inout)         :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num, 2)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)
  
  integer                        :: i, ii, j, k, l, h(0:2,2), p(0:4,2), nt
  integer(bit_kind)              :: perMask(N_int, 2), mobMask(N_int, 2), negMask(N_int, 2)
  integer                     :: phasemask(2,N_int*bit_kind_size)
!   logical :: bandon
!   
!   bandon = .false.
  PROVIDE psi_selectors_coef_transp
  mat = 0d0
  
  do i=1,N_int
    negMask(i,1) = not(mask(i,1))
    negMask(i,2) = not(mask(i,2))
  end do

  do i=1, N_sel ! interesting(0)
    !i = interesting(ii)
    if (interesting(i) < 0) then
      stop 'prefetch interesting(i)'
    endif

    
    mobMask(1,1) = iand(negMask(1,1), det(1,1,i))
    mobMask(1,2) = iand(negMask(1,2), det(1,2,i))
    nt = popcnt(mobMask(1, 1)) + popcnt(mobMask(1, 2))

    if(nt > 4) cycle

    do j=2,N_int
      mobMask(j,1) = iand(negMask(j,1), det(j,1,i))
      mobMask(j,2) = iand(negMask(j,2), det(j,2,i))
      nt = nt + popcnt(mobMask(j, 1)) + popcnt(mobMask(j, 2))
    end do

    if(nt > 4) cycle
    
    if (interesting(i) == i_gen) then
        if(sp == 3) then
          do j=1,mo_tot_num
            do k=1,mo_tot_num
              banned(j,k,2) = banned(k,j,1)
            enddo
          enddo
        else
          do k=1,mo_tot_num
          do l=k+1,mo_tot_num
            banned(l,k,1) = banned(k,l,1)
          end do
          end do
        end if
    end if

    call bitstring_to_list_in_selection(mobMask(1,1), p(1,1), p(0,1), N_int)
    call bitstring_to_list_in_selection(mobMask(1,2), p(1,2), p(0,2), N_int)
    
    perMask(1,1) = iand(mask(1,1), not(det(1,1,i)))
    perMask(1,2) = iand(mask(1,2), not(det(1,2,i)))
    do j=2,N_int
      perMask(j,1) = iand(mask(j,1), not(det(j,1,i)))
      perMask(j,2) = iand(mask(j,2), not(det(j,2,i)))
    end do

    call bitstring_to_list_in_selection(perMask(1,1), h(1,1), h(0,1), N_int)
    call bitstring_to_list_in_selection(perMask(1,2), h(1,2), h(0,2), N_int)

    if (interesting(i) >= i_gen) then
        call get_mask_phase(psi_det_sorted(1,1,interesting(i)), phasemask)
        if(nt == 4) then
          call get_d2(det(1,1,i), phasemask, bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, interesting(i)))
        else if(nt == 3) then
          call get_d1(det(1,1,i), phasemask, bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, interesting(i)))
        else
          call get_d0(det(1,1,i), phasemask, bannedOrb, banned, mat, mask, h, p, sp, psi_selectors_coef_transp(1, interesting(i)))
        end if
    else 
        if(nt == 4) call past_d2(banned, p, sp)
        if(nt == 3) call past_d1(bannedOrb, p)
    end if
  end do
end 


subroutine get_d2(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: mask(N_int, 2), gen(N_int, 2)
  integer, intent(in) :: phasemask(2,N_int*bit_kind_size)
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
      if(bannedOrb(puti, mi)) return
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
          mat(:, putj, puti) += coefs(:) * hij
        else
          mat(:, puti, putj) += coefs(:) * hij
        end if
      end do
    else
      h1 = h(1,1)
      h2 = h(1,2)
      do j = 1,2
        putj = p(j, 2)
        if(bannedOrb(putj, 2)) cycle
        p2 = p(turn2(j), 2)
        do i = 1,2
          puti = p(i, 1)
          
          if(banned(puti,putj,bant) .or. bannedOrb(puti,1)) cycle
          p1 = p(turn2(i), 1)
          
          hij = mo_bielec_integral(p1, p2, h1, h2) * get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
          mat(:, puti, putj) += coefs(:) * hij
        end do
      end do
    end if

  else
    if(tip == 0) then
      h1 = h(1, ma)
      h2 = h(2, ma)
      do i=1,3
      puti = p(i, ma)
      if(bannedOrb(puti,ma)) cycle
      do j=i+1,4
        putj = p(j, ma)
        if(bannedOrb(putj,ma)) cycle
        if(banned(puti,putj,1)) cycle
        
        i1 = turn2d(1, i, j)
        i2 = turn2d(2, i, j)
        p1 = p(i1, ma)
        p2 = p(i2, ma)
        hij = (mo_bielec_integral(p1, p2, h1, h2) - mo_bielec_integral(p2,p1, h1, h2)) * get_phase_bi(phasemask, ma, ma, h1, p1, h2, p2)
        mat(:, puti, putj) += coefs(:) * hij
      end do
      end do
    else if(tip == 3) then
      h1 = h(1, mi)
      h2 = h(1, ma)
      p1 = p(1, mi)
      do i=1,3
        puti = p(turn3(1,i), ma)
        if(bannedOrb(puti,ma)) cycle
        putj = p(turn3(2,i), ma)
        if(bannedOrb(putj,ma)) cycle
        if(banned(puti,putj,1)) cycle
        p2 = p(i, ma)
        
        hij = mo_bielec_integral(p1, p2, h1, h2) * get_phase_bi(phasemask, mi, ma, h1, p1, h2, p2)
        mat(:, min(puti, putj), max(puti, putj)) += coefs(:) * hij
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
        mat(:, puti, putj) += coefs(:) * hij
      end if
    end if
  end if
end 


subroutine get_d1(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in)  :: mask(N_int, 2), gen(N_int, 2)
  integer,intent(in)          :: phasemask(2,N_int*bit_kind_size)
  logical, intent(in)            :: bannedOrb(mo_tot_num, 2), banned(mo_tot_num, mo_tot_num,2)
  integer(bit_kind)              :: det(N_int, 2)
  double precision, intent(in)   :: coefs(N_states)
  double precision, intent(inout) :: mat(N_states, mo_tot_num, mo_tot_num)
  integer, intent(in)            :: h(0:2,2), p(0:4,2), sp
  double precision               :: hij, tmp_row(N_states, mo_tot_num), tmp_row2(N_states, mo_tot_num)
  double precision, external     :: get_phase_bi, mo_bielec_integral
  logical                        :: ok

  logical, allocatable           :: lbanned(:,:)
  integer                        :: puti, putj, ma, mi, s1, s2, i, i1, i2, j
  integer                        :: hfix, pfix, h1, h2, p1, p2, ib
  
  integer, parameter             :: turn2(2) = (/2,1/)
  integer, parameter             :: turn3(2,3) = reshape((/2,3,  1,3, 1,2/), (/2,3/))
  
  integer                        :: bant
  
  
  allocate (lbanned(mo_tot_num, 2))
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
        tmp_row(:,puti) += hij * coefs(:)
      end if
      
      putj = p2
      if(.not. banned(putj,puti,bant)) then
        hij = mo_bielec_integral(p1,pfix,hfix,puti) * get_phase_bi(phasemask, ma, mi, hfix, p1, puti, pfix)
        tmp_row2(:,puti) += hij * coefs(:)
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
          tmp_row(:,putj) += hij * coefs(:)
        end do
        do putj=hfix+1,mo_tot_num
          if(lbanned(putj,ma) .or. banned(puti,putj,1)) cycle
          hij = (mo_bielec_integral(p1, p2, hfix, putj)-mo_bielec_integral(p2,p1,hfix,putj)) * get_phase_bi(phasemask, ma, ma, hfix, p1, putj, p2)
          tmp_row(:,putj) += hij * coefs(:)
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
          tmp_row(:,puti) += hij * coefs(:)
        end if
        
        putj = p1
        if(.not. banned(puti,putj,1)) then
          hij = mo_bielec_integral(pfix, p2, hfix, puti) * get_phase_bi(phasemask, mi, ma, hfix, pfix, puti, p2)
          tmp_row2(:,puti) += hij * coefs(:)
        end if
      end do
      mat(:,:p2-1,p2) += tmp_row(:,:p2-1)
      mat(:,p2,p2:) += tmp_row(:,p2:)
      mat(:,:p1-1,p1) += tmp_row2(:,:p1-1)
      mat(:,p1,p1:) += tmp_row2(:,p1:)
    end if
  end if
  deallocate(lbanned)

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
        mat(:, p1, p2) += coefs(:) * hij
      end do
    end do
end 




subroutine get_d0(gen, phasemask, bannedOrb, banned, mat, mask, h, p, sp, coefs)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: gen(N_int, 2), mask(N_int, 2)
  integer, intent(in) :: phasemask(2,N_int*bit_kind_size)
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
          phase = get_phase_bi(phasemask, 1, 2, h1, p1, h2, p2)
          hij = mo_bielec_integral(p1, p2, h1, h2) * phase
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
end 
 

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
end 


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
end 



subroutine spot_isinwf(mask, det, i_gen, N, banned, fullMatch, interesting)
  use bitmasks
  implicit none
  
  integer, intent(in) :: i_gen, N
  integer, intent(in) :: interesting(0:N)
  integer(bit_kind),intent(in) :: mask(N_int, 2), det(N_int, 2, N)
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

    call bitstring_to_list_in_selection(myMask(1,1), list(1), na, N_int)
    call bitstring_to_list_in_selection(myMask(1,2), list(na+1), nb, N_int)
    banned(list(1), list(2)) = .true.
  end do genl
end 


subroutine bitstring_to_list_in_selection( string, list, n_elements, Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Gives the inidices(+1) of the bits set to 1 in the bit string
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: string(Nint)
  integer, intent(out)           :: list(Nint*bit_kind_size)
  integer, intent(out)           :: n_elements
  
  integer                        :: i, ishift
  integer(bit_kind)              :: l
  
  n_elements = 0
  ishift = 2
  do i=1,Nint
    l = string(i)
    do while (l /= 0_bit_kind)
      n_elements = n_elements+1
      list(n_elements) = ishift+popcnt(l-1_bit_kind) - popcnt(l)
      l = iand(l,l-1_bit_kind)
    enddo
    ishift = ishift + bit_kind_size
  enddo
  
end
