

subroutine $subroutine_diexc(key_in, key_prev, hole_1,particl_1, hole_2, particl_2, fock_diag_tmp, i_generator, iproc_in $parameters )
  
  integer(bit_kind), intent(in)         :: key_in(N_int, 2), hole_1(N_int, 2), hole_2(N_int, 2)
  integer(bit_kind), intent(in)         :: particl_1(N_int, 2), particl_2(N_int, 2)
  integer(bit_kind)                     :: p1_mask(N_int, 2), p2_mask(N_int, 2), tmp
  integer,intent(in)                    :: i_generator,iproc_in
  integer(bit_kind)                     :: status(N_int*bit_kind_size, 2)
  integer                               :: highest, p1,p2,sp,ni,i,mi,nt,ns
  double precision, intent(in)          :: fock_diag_tmp(2,mo_tot_num+1)
  integer(bit_kind), intent(in)         :: key_prev(N_int, 2, *)
  PROVIDE N_int
  PROVIDE N_det
 
  $declarations
  
  
  highest = 0
  status(:,:) = 0
  do sp=1,2
    do ni=1,N_int
      do i=1,bit_kind_size
        if(iand(1,ishft(key_in(ni, sp), -(i-1))) == 0) then
          cycle
        end if
        mi = (ni-1)*bit_kind_size+i
        status(mi, sp) = iand(1,ishft(hole_1(ni, sp), -(i-1)))
        status(mi, sp) = status(mi, sp) + 2*iand(1,ishft(hole_2(ni, sp), -(i-1)))
        if(status(mi, sp) /= 0 .and. mi > highest) then
          highest = mi
        end if
      end do
    end do
  end do
  
! ! GEL D'ELECTRONS
! !  nt = 0
!   do i=1,i_generator-1
!     if(key_in(1,1) == key_prev(1,1,i)) then
!       tmp = xor(key_in(1,2), key_prev(1,2,i))
!       if(popcnt(tmp) == 2) then
!         ns = 1+trailz(iand(tmp, key_in(1,2)))
! !         if(status(ns, 2) /= 0) then
! !           nt += 1
! !         end if
!         status(ns, 2) = 0
!       end if
!     else if(key_in(1,2) == key_prev(1,2,i)) then
!       tmp = xor(key_in(1,1), key_prev(1,1,i))
!       if(popcnt(tmp) == 2) then
!         ns = 1+trailz(iand(tmp, key_in(1,1)))
! !         if(status(ns, 1) /= 0) then
! !           nt += 1
! !         end if
!         status(ns, 1) = 0
!       end if
!     end if
!   end do
! !  print *, "nt", nt, i_generator
  
  
  do sp=1,2
    do p1=1,highest
      if(status(p1, sp) == 0) then
        cycle
      end if
      do p2=1,highest
        if(status(p2, sp) == 0) then
          cycle
        end if
        if((status(p1, sp) == 1 .and. status(p2, sp) > 1) .or. &
            (status(p1, sp) == 2 .and. status(p2, sp) == 3) .or. &
            (status(p1, sp) == 3 .and. status(p2, sp) == 3 .and. p2 > p1)) then
          call $subroutine_diexcP(key_in, sp, p1, particl_1, sp, p2, particl_2, fock_diag_tmp, i_generator, iproc_in $parameters )
        end if
      end do
    end do
  end do
  do p1=1,highest
    if(status(p1, 1) == 0) then
      cycle
    end if
    do p2=1,highest
      if(status(p2, 2) == 0) then
        cycle
      end if
      if((status(p1, 1) == 3) .or. &
          (status(p1, 1) == 1 .and. status(p2, 2) >= 2) .or. &
          (status(p1, 1) == 2 .and. status(p2, 2) /= 2)) then
          
          call $subroutine_diexcP(key_in, 1, p1, particl_1, 2, p2, particl_2, fock_diag_tmp, i_generator, iproc_in $parameters )
      end if
    end do
  end do
end subroutine


subroutine $subroutine_diexcP(key_in, fs1, fh1, particl_1, fs2, fh2, particl_2, fock_diag_tmp, i_generator, iproc_in $parameters )
  implicit none
  integer(bit_kind), intent(in)         :: key_in(N_int, 2), particl_1(N_int, 2), particl_2(N_int, 2)
  double precision, intent(in)          :: fock_diag_tmp(2,mo_tot_num+1)
  integer(bit_kind)                     :: p1_mask(N_int, 2), p2_mask(N_int, 2), key_mask(N_int, 2)
  integer,intent(in)                    :: fs1,fs2,i_generator,iproc_in, fh1,fh2
  integer(bit_kind)                     :: miniList(N_int, 2, N_det)
  integer                               :: n_minilist, n_alpha, n_beta, deg(2), i, ni
  $declarations
  integer(bit_kind), parameter :: one = 1_bit_kind
  
  p1_mask(:,:) = 0_bit_kind
  p2_mask(:,:) = 0_bit_kind
  p1_mask(ishft(fh1-1,-bit_kind_shift) + 1, fs1) = ishft(one,iand(fh1-1,bit_kind_size-1))
  p2_mask(ishft(fh2-1,-bit_kind_shift) + 1, fs2) = ishft(one,iand(fh2-1,bit_kind_size-1))
  
  key_mask(:,:) = key_in(:,:)

  key_mask(ishft(fh1-1,-bit_kind_shift) + 1, fs1) -= ishft(one,iand(fh1-1,bit_kind_size-1))
  key_mask(ishft(fh2-1,-bit_kind_shift) + 1, fs2) -= ishft(one,iand(fh2-1,bit_kind_size-1))
  
      
  call $subroutine_diexcOrg(key_in, key_mask, p1_mask, particl_1, p2_mask, particl_2, fock_diag_tmp, i_generator, iproc_in $parameters )
end subroutine


subroutine $subroutine_diexcOrg(key_in,key_mask,hole_1,particl_1,hole_2, particl_2, fock_diag_tmp, i_generator, iproc_in $parameters )
  use omp_lib
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Generate all double excitations of key_in using the bit masks of holes and
  ! particles.
  ! Assume N_int is already provided.
  END_DOC
  integer,parameter              :: size_max = $size_max
  $declarations
  integer          ,intent(in)   :: i_generator
  integer(bit_kind),intent(in)   :: key_in(N_int,2), key_mask(N_int, 2)
  integer(bit_kind),allocatable  :: keys_out(:,:,:)
  integer(bit_kind), intent(in)  :: hole_1(N_int,2), particl_1(N_int,2)
  integer(bit_kind), intent(in)  :: hole_2(N_int,2), particl_2(N_int,2)
  integer, intent(in)            :: iproc_in
  double precision, intent(in)   :: fock_diag_tmp(2,mo_tot_num+1)
  integer(bit_kind), allocatable :: hole_save(:,:)
  integer(bit_kind), allocatable :: key(:,:),hole(:,:), particle(:,:)
  integer(bit_kind), allocatable :: hole_tmp(:,:), particle_tmp(:,:)
  integer(bit_kind), allocatable :: key_union_hole_part(:)
  integer                        :: ii,i,jj,j,k,ispin,l
  integer, allocatable           :: occ_particle(:,:), occ_hole(:,:)
  integer, allocatable           :: occ_particle_tmp(:,:), occ_hole_tmp(:,:)
  integer                        :: kk,pp,other_spin,key_idx
  integer                        :: N_elec_in_key_hole_1(2),N_elec_in_key_part_1(2)
  integer                        :: N_elec_in_key_hole_2(2),N_elec_in_key_part_2(2)
  
  double precision               :: mo_bielec_integral
  logical                        :: is_a_two_holes_two_particles
  integer, allocatable           :: ia_ja_pairs(:,:,:)
  integer, allocatable           :: ib_jb_pairs(:,:)
  double precision               :: diag_H_mat_elem
  integer                        :: iproc
  integer                        :: jtest_vvvv
  integer(omp_lock_kind), save   :: lck, ifirst=0
  if (ifirst == 0) then
!$    call omp_init_lock(lck)
    ifirst=1
  endif
  
  logical :: check_double_excitation 
  logical :: is_a_1h1p
  logical :: b_cycle
  check_double_excitation = .True.
  iproc = iproc_in


  $initialization
  
  $omp_parallel
!$ iproc = omp_get_thread_num()
  allocate (keys_out(N_int,2,size_max), hole_save(N_int,2),          &
      key(N_int,2),hole(N_int,2), particle(N_int,2), hole_tmp(N_int,2),&
      particle_tmp(N_int,2), occ_particle(N_int*bit_kind_size,2),    &
      occ_hole(N_int*bit_kind_size,2), occ_particle_tmp(N_int*bit_kind_size,2),&
      occ_hole_tmp(N_int*bit_kind_size,2),key_union_hole_part(N_int))

  $init_thread
  
  
  
  !!!! First couple hole particle
  do j = 1, N_int
    hole(j,1) = iand(hole_1(j,1),key_in(j,1))
    hole(j,2) = iand(hole_1(j,2),key_in(j,2))
    particle(j,1) = iand(xor(particl_1(j,1),key_in(j,1)),particl_1(j,1))
    particle(j,2) = iand(xor(particl_1(j,2),key_in(j,2)),particl_1(j,2))
  enddo
  call bitstring_to_list_ab(particle,occ_particle,N_elec_in_key_part_1,N_int)
  call bitstring_to_list_ab(hole,occ_hole,N_elec_in_key_hole_1,N_int)
  allocate (ia_ja_pairs(2,0:(elec_alpha_num)*mo_tot_num,2),          &
            ib_jb_pairs(2,0:(elec_alpha_num)*mo_tot_num))
  
  do ispin=1,2
    i=0
    do ii=N_elec_in_key_hole_1(ispin),1,-1             ! hole
      i_a = occ_hole(ii,ispin)
      ASSERT (i_a > 0)
      ASSERT (i_a <= mo_tot_num)
      
      do jj=1,N_elec_in_key_part_1(ispin)              !particle
        j_a = occ_particle(jj,ispin)
        ASSERT (j_a > 0)
        ASSERT (j_a <= mo_tot_num)
        i += 1
        ia_ja_pairs(1,i,ispin) = i_a
        ia_ja_pairs(2,i,ispin) = j_a
      enddo
    enddo
    ia_ja_pairs(1,0,ispin) = i
  enddo
  
  key_idx = 0
  
  integer                        :: i_a,j_a,i_b,j_b,k_a,l_a,k_b,l_b
  integer(bit_kind)              :: test(N_int,2)
  double precision               :: accu
  logical, allocatable           :: array_pairs(:,:)
  allocate(array_pairs(mo_tot_num,mo_tot_num))
  accu = 0.d0
  do ispin=1,2
    other_spin = iand(ispin,1)+1
    if (abort_here) then
      exit
    endif
    $omp_do
    do ii=1,ia_ja_pairs(1,0,ispin)
      if (abort_here) then
        cycle
      endif
      i_a = ia_ja_pairs(1,ii,ispin)
      ASSERT (i_a > 0)
      ASSERT (i_a <= mo_tot_num)
      j_a = ia_ja_pairs(2,ii,ispin)
      ASSERT (j_a > 0)
      ASSERT (j_a <= mo_tot_num)
      hole = key_in
      k = ishft(i_a-1,-bit_kind_shift)+1
      j = i_a-ishft(k-1,bit_kind_shift)-1
      hole(k,ispin) = ibclr(hole(k,ispin),j)
      k_a = ishft(j_a-1,-bit_kind_shift)+1
      l_a = j_a-ishft(k_a-1,bit_kind_shift)-1
      hole(k_a,ispin) = ibset(hole(k_a,ispin),l_a)
      
      !!!! Second couple hole particle
      do j = 1, N_int
        hole_tmp(j,1) = iand(hole_2(j,1),hole(j,1))
        hole_tmp(j,2) = iand(hole_2(j,2),hole(j,2))
        particle_tmp(j,1) = iand(xor(particl_2(j,1),hole(j,1)),particl_2(j,1))
        particle_tmp(j,2) = iand(xor(particl_2(j,2),hole(j,2)),particl_2(j,2))
      enddo
      
      call bitstring_to_list_ab(particle_tmp,occ_particle_tmp,N_elec_in_key_part_2,N_int)
      call bitstring_to_list_ab(hole_tmp,occ_hole_tmp,N_elec_in_key_hole_2,N_int)
      
      !   hole = a^(+)_j_a(ispin) a_i_a(ispin)|key_in> : mono exc :: orb(i_a,ispin) --> orb(j_a,ispin)
      hole_save = hole

      ! Build array of the non-zero integrals of second excitation
      $filter_integrals

      if (ispin == 1) then
        integer                        :: jjj
        
        i=0
        do kk = 1,N_elec_in_key_hole_2(other_spin)
          i_b = occ_hole_tmp(kk,other_spin)
          ASSERT (i_b > 0)
          ASSERT (i_b <= mo_tot_num)
          do jjj=1,N_elec_in_key_part_2(other_spin)     ! particle
            j_b = occ_particle_tmp(jjj,other_spin)
            ASSERT (j_b > 0)
            ASSERT (j_b <= mo_tot_num)
            if (array_pairs(i_b,j_b)) then
              $filter_vvvv_excitation
              i+= 1
              ib_jb_pairs(1,i) = i_b
              ib_jb_pairs(2,i) = j_b
            endif
          enddo
        enddo
        ib_jb_pairs(1,0) = i

        do kk = 1,ib_jb_pairs(1,0)
          hole = hole_save
          i_b = ib_jb_pairs(1,kk)
          j_b = ib_jb_pairs(2,kk)
          k = ishft(i_b-1,-bit_kind_shift)+1
          j = i_b-ishft(k-1,bit_kind_shift)-1
          hole(k,other_spin) = ibclr(hole(k,other_spin),j)
          key = hole
          k = ishft(j_b-1,-bit_kind_shift)+1
          l = j_b-ishft(k-1,bit_kind_shift)-1
          key(k,other_spin) = ibset(key(k,other_spin),l)
          $filter2h2p
          $filter_only_1h1p_double
          key_idx += 1
          do k=1,N_int
            keys_out(k,1,key_idx) = key(k,1)
            keys_out(k,2,key_idx) = key(k,2)
          enddo
          ASSERT (key_idx <= size_max)
          if (key_idx == size_max) then
            $keys_work
            key_idx = 0
          endif
          if (abort_here) then
            exit
          endif
        enddo
      endif

      !   does all the mono excitations of the same spin
      i=0
      do kk = 1,N_elec_in_key_hole_2(ispin)
        i_b = occ_hole_tmp(kk,ispin)
        if (i_b <= i_a.or.i_b == j_a) cycle
        ASSERT (i_b > 0)
        ASSERT (i_b <= mo_tot_num)
        do jjj=1,N_elec_in_key_part_2(ispin)     ! particule
          j_b = occ_particle_tmp(jjj,ispin)
          ASSERT (j_b > 0)
          ASSERT (j_b <= mo_tot_num)
          if (j_b <= j_a) cycle
          if (array_pairs(i_b,j_b)) then
            $filter_vvvv_excitation
            i+= 1
            ib_jb_pairs(1,i) = i_b
            ib_jb_pairs(2,i) = j_b
          endif
        enddo
      enddo
      ib_jb_pairs(1,0) = i

      do kk = 1,ib_jb_pairs(1,0)
        hole = hole_save
        i_b = ib_jb_pairs(1,kk)
        j_b = ib_jb_pairs(2,kk)
        k = ishft(i_b-1,-bit_kind_shift)+1
        j = i_b-ishft(k-1,bit_kind_shift)-1
        hole(k,ispin) = ibclr(hole(k,ispin),j)
        key = hole
        k = ishft(j_b-1,-bit_kind_shift)+1
        l = j_b-ishft(k-1,bit_kind_shift)-1
        key(k,ispin) = ibset(key(k,ispin),l)
        $filter2h2p
        $filter_only_1h1p_double
        key_idx += 1
        do k=1,N_int
          keys_out(k,1,key_idx) = key(k,1)
          keys_out(k,2,key_idx) = key(k,2)
        enddo
        ASSERT (key_idx <= size_max)
        if (key_idx == size_max) then
          $keys_work
          key_idx = 0
        endif
        if (abort_here) then
          exit
        endif
      enddo ! kk

    enddo  ! ii
    $omp_enddo
  enddo   ! ispin
  $keys_work
  $deinit_thread
  deallocate (ia_ja_pairs, ib_jb_pairs,                              &
      keys_out, hole_save,                                           &
      key,hole, particle, hole_tmp,                                  &
      particle_tmp, occ_particle,                                    &
      occ_hole, occ_particle_tmp,                                    &
      occ_hole_tmp,array_pairs,key_union_hole_part)
  $omp_end_parallel
  $finalization
end

subroutine $subroutine_monoexc(key_in, hole_1,particl_1,fock_diag_tmp,i_generator,iproc_in $parameters )
  use omp_lib
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Generate all single excitations of key_in using the bit masks of holes and
  ! particles.
  ! Assume N_int is already provided.
  END_DOC
  integer,parameter              :: size_max = $size_max
  $declarations
  integer          ,intent(in)   :: i_generator
  integer(bit_kind),intent(in)   :: key_in(N_int,2)
  integer(bit_kind),intent(in)   :: hole_1(N_int,2), particl_1(N_int,2)
  integer, intent(in)            :: iproc_in
  double precision, intent(in)   :: fock_diag_tmp(2,mo_tot_num+1)
  integer(bit_kind),allocatable  :: keys_out(:,:,:)
  integer(bit_kind),allocatable  :: hole_save(:,:)
  integer(bit_kind),allocatable  :: key(:,:),hole(:,:), particle(:,:)
  integer(bit_kind),allocatable  :: hole_tmp(:,:), particle_tmp(:,:)
  integer(bit_kind),allocatable  :: hole_2(:,:), particl_2(:,:)
  integer                        :: ii,i,jj,j,k,ispin,l
  integer,allocatable            :: occ_particle(:,:), occ_hole(:,:)
  integer,allocatable            :: occ_particle_tmp(:,:), occ_hole_tmp(:,:)
  integer,allocatable            :: ib_jb_pairs(:,:)
  integer                        :: kk,pp,other_spin,key_idx
  integer                        :: N_elec_in_key_hole_1(2),N_elec_in_key_part_1(2)
  integer                        :: N_elec_in_key_hole_2(2),N_elec_in_key_part_2(2)
  logical                        :: is_a_two_holes_two_particles
  integer(bit_kind), allocatable :: key_union_hole_part(:)
  
  integer, allocatable           :: ia_ja_pairs(:,:,:)
  logical, allocatable           :: array_pairs(:,:)
  double precision               :: diag_H_mat_elem
  integer(omp_lock_kind), save   :: lck, ifirst=0
  integer                        :: iproc
  
  integer(bit_kind)              :: key_mask(N_int, 2)
  
  logical :: check_double_excitation 
  logical :: is_a_1h1p
  
  key_mask(:,:) = 0_bit_kind
  
  iproc = iproc_in

  check_double_excitation = .True.
  $check_double_excitation
  

  if (ifirst == 0) then
    ifirst=1
!$    call omp_init_lock(lck)
  endif
  
  $initialization
  
  $omp_parallel
!$ iproc = omp_get_thread_num()
  allocate (keys_out(N_int,2,size_max), hole_save(N_int,2),          &
      key(N_int,2),hole(N_int,2), particle(N_int,2), hole_tmp(N_int,2),&
      particle_tmp(N_int,2), occ_particle(N_int*bit_kind_size,2),    &
      occ_hole(N_int*bit_kind_size,2), occ_particle_tmp(N_int*bit_kind_size,2),&
      occ_hole_tmp(N_int*bit_kind_size,2),key_union_hole_part(N_int))
  $init_thread
  !!!! First couple hole particle
  do j = 1, N_int
    hole(j,1) = iand(hole_1(j,1),key_in(j,1))
    hole(j,2) = iand(hole_1(j,2),key_in(j,2))
    particle(j,1) = iand(xor(particl_1(j,1),key_in(j,1)),particl_1(j,1))
    particle(j,2) = iand(xor(particl_1(j,2),key_in(j,2)),particl_1(j,2))
  enddo
  
  call bitstring_to_list_ab(particle,occ_particle,N_elec_in_key_part_1,N_int)
  call bitstring_to_list_ab(hole,occ_hole,N_elec_in_key_hole_1,N_int)
  allocate (ia_ja_pairs(2,0:(elec_alpha_num)*mo_tot_num,2))
  
  do ispin=1,2
    i=0
    do ii=N_elec_in_key_hole_1(ispin),1,-1             ! hole
      i_a = occ_hole(ii,ispin)
      do jj=1,N_elec_in_key_part_1(ispin)                            !particule
        j_a = occ_particle(jj,ispin)
        i += 1
        ia_ja_pairs(1,i,ispin) = i_a
        ia_ja_pairs(2,i,ispin) = j_a
      enddo
    enddo
    ia_ja_pairs(1,0,ispin) = i
  enddo
  
  key_idx = 0
  
  integer                        :: i_a,j_a,i_b,j_b,k_a,l_a,k_b,l_b
  integer(bit_kind)              :: test(N_int,2)
  double precision               :: accu
  accu = 0.d0
  do ispin=1,2
    other_spin = iand(ispin,1)+1
    $omp_do
    do ii=1,ia_ja_pairs(1,0,ispin)
      i_a = ia_ja_pairs(1,ii,ispin)
      j_a = ia_ja_pairs(2,ii,ispin)
      hole = key_in
      k = ishft(i_a-1,-bit_kind_shift)+1
      j = i_a-ishft(k-1,bit_kind_shift)-1
  $filterhole
      hole(k,ispin) = ibclr(hole(k,ispin),j)
      k_a = ishft(j_a-1,-bit_kind_shift)+1
      l_a = j_a-ishft(k_a-1,bit_kind_shift)-1
  $filterparticle
      hole(k_a,ispin) = ibset(hole(k_a,ispin),l_a)
      $filter2h2p
      $filter_only_1h1p_single
      key_idx += 1
      do k=1,N_int
        keys_out(k,1,key_idx) = hole(k,1)
        keys_out(k,2,key_idx) = hole(k,2)
      enddo
      if (key_idx == size_max) then
        $keys_work
        key_idx = 0
      endif
    enddo  ! ii
    $omp_enddo
  enddo   ! ispin
  $keys_work
  $deinit_thread
  deallocate (ia_ja_pairs, &
      keys_out, hole_save,          &
      key,hole, particle, hole_tmp,&
      particle_tmp, occ_particle,    &
      occ_hole, occ_particle_tmp,&
      occ_hole_tmp,key_union_hole_part)
  $omp_end_parallel
  $finalization
  
end


subroutine $subroutine($params_main)
  implicit none
  use omp_lib
  use bitmasks
  BEGIN_DOC
  ! Calls H_apply on the HF determinant and selects all connected single and double
  ! excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.
  END_DOC
  
  $decls_main
  
  integer                        :: i_generator, nmax
  double precision               :: wall_0, wall_1
  integer(omp_lock_kind)         :: lck
  integer(bit_kind), allocatable :: mask(:,:,:)
  integer                        :: ispin, k
  integer                        :: iproc
  double precision, allocatable  :: fock_diag_tmp(:,:)

  $initialization
  PROVIDE H_apply_buffer_allocated mo_bielec_integrals_in_map psi_det_generators psi_coef_generators

  
  nmax = mod( N_det_generators,nproc )

  !$ call omp_init_lock(lck)
  call start_progress(N_det_generators,'Selection (norm)',0.d0)

  call wall_time(wall_0)

  iproc = 0
  allocate( mask(N_int,2,6), fock_diag_tmp(2,mo_tot_num+1) )
  do i_generator=1,nmax

    progress_bar(1) = i_generator

    if (abort_here) then
      exit
    endif
    $skip

    ! Compute diagonal of the Fock matrix
    call build_fock_tmp(fock_diag_tmp,psi_det_generators(1,1,i_generator),N_int)

    ! Create bit masks for holes and particles
    do ispin=1,2
      do k=1,N_int
        mask(k,ispin,s_hole) =                                      &
            iand(generators_bitmask(k,ispin,s_hole,i_bitmask_gen),  &
            psi_det_generators(k,ispin,i_generator) )
        mask(k,ispin,s_part) =                                      &
            iand(generators_bitmask(k,ispin,s_part,i_bitmask_gen),  &
            not(psi_det_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole1) =                                      &
            iand(generators_bitmask(k,ispin,d_hole1,i_bitmask_gen),  &
            psi_det_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part1) =                                      &
            iand(generators_bitmask(k,ispin,d_part1,i_bitmask_gen),  &
            not(psi_det_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole2) =                                      &
            iand(generators_bitmask(k,ispin,d_hole2,i_bitmask_gen),  &
            psi_det_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part2) =                                      &
            iand(generators_bitmask(k,ispin,d_part2,i_bitmask_gen),  &
            not(psi_det_generators(k,ispin,i_generator)) )
      enddo
    enddo
    if($do_double_excitations)then
     call $subroutine_diexc(psi_det_generators(1,1,i_generator),      &
         psi_det_generators(1,1,1),                                   &
         mask(1,1,d_hole1), mask(1,1,d_part1),                        &
         mask(1,1,d_hole2), mask(1,1,d_part2),                        &
         fock_diag_tmp, i_generator, iproc $params_post)
    endif
    if($do_mono_excitations)then
     call $subroutine_monoexc(psi_det_generators(1,1,i_generator),    &
         mask(1,1,s_hole ), mask(1,1,s_part ),                        &
         fock_diag_tmp, i_generator, iproc $params_post)
    endif
    call wall_time(wall_1)
    $printout_always
    if (wall_1 - wall_0 > 2.d0) then
        $printout_now
        wall_0 = wall_1
    endif
  enddo

  deallocate( mask, fock_diag_tmp )

  !$OMP PARALLEL DEFAULT(SHARED) &
  !$OMP PRIVATE(i_generator,wall_1,wall_0,ispin,k,mask,iproc,fock_diag_tmp) 
  call wall_time(wall_0)
  !$ iproc = omp_get_thread_num()
  allocate( mask(N_int,2,6), fock_diag_tmp(2,mo_tot_num+1) )
  !$OMP DO SCHEDULE(dynamic,1)
  do i_generator=nmax+1,N_det_generators
    if (iproc == 0) then
      progress_bar(1) = i_generator
    endif
    if (abort_here) then
      cycle
    endif
    $skip

    ! Compute diagonal of the Fock matrix
    call build_fock_tmp(fock_diag_tmp,psi_det_generators(1,1,i_generator),N_int)

    ! Create bit masks for holes and particles
    do ispin=1,2
      do k=1,N_int
        mask(k,ispin,s_hole) =                                      &
            iand(generators_bitmask(k,ispin,s_hole,i_bitmask_gen),  &
            psi_det_generators(k,ispin,i_generator) )
        mask(k,ispin,s_part) =                                      &
            iand(generators_bitmask(k,ispin,s_part,i_bitmask_gen),  &
            not(psi_det_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole1) =                                      &
            iand(generators_bitmask(k,ispin,d_hole1,i_bitmask_gen),  &
            psi_det_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part1) =                                      &
            iand(generators_bitmask(k,ispin,d_part1,i_bitmask_gen),  &
            not(psi_det_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole2) =                                      &
            iand(generators_bitmask(k,ispin,d_hole2,i_bitmask_gen),  &
            psi_det_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part2) =                                      &
            iand(generators_bitmask(k,ispin,d_part2,i_bitmask_gen),  &
            not (psi_det_generators(k,ispin,i_generator)) )
      enddo
    enddo

    if($do_double_excitations)then
      call $subroutine_diexc(psi_det_generators(1,1,i_generator),    &
        psi_det_generators(1,1,1),                                   &
        mask(1,1,d_hole1), mask(1,1,d_part1),                        &
        mask(1,1,d_hole2), mask(1,1,d_part2),                        &
        fock_diag_tmp, i_generator, iproc $params_post)
    endif
    if($do_mono_excitations)then
      call $subroutine_monoexc(psi_det_generators(1,1,i_generator),  &
        mask(1,1,s_hole ), mask(1,1,s_part ),                        &
        fock_diag_tmp, i_generator, iproc $params_post)
    endif
    !$ call omp_set_lock(lck)
    call wall_time(wall_1)
    $printout_always
    if (wall_1 - wall_0 > 2.d0) then
        $printout_now
        wall_0 = wall_1
    endif
    !$ call omp_unset_lock(lck)
  enddo
  !$OMP END DO 
  deallocate( mask, fock_diag_tmp )
  !$OMP END PARALLEL
  !$ call omp_destroy_lock(lck)

  abort_here = abort_all
  call stop_progress
  
  $copy_buffer
  $generate_psi_guess
  
end

