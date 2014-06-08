subroutine $subroutine_diexc(key_in, hole_1,particl_1, hole_2, particl_2, i_generator $parameters )
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
  integer(bit_kind),intent(in)   :: key_in(N_int,2)
  integer(bit_kind),allocatable  :: keys_out(:,:,:)
  integer(bit_kind), intent(in)  :: hole_1(N_int,2), particl_1(N_int,2)
  integer(bit_kind), intent(in)  :: hole_2(N_int,2), particl_2(N_int,2)
  integer(bit_kind), allocatable :: hole_save(:,:)
  integer(bit_kind), allocatable :: key(:,:),hole(:,:), particle(:,:)
  integer(bit_kind), allocatable :: hole_tmp(:,:), particle_tmp(:,:)
  integer                        :: ii,i,jj,j,k,ispin,l
  integer, allocatable           :: occ_particle(:,:), occ_hole(:,:)
  integer, allocatable           :: occ_particle_tmp(:,:), occ_hole_tmp(:,:)
  integer                        :: kk,pp,other_spin,key_idx
  integer                        :: N_elec_in_key_hole_1(2),N_elec_in_key_part_1(2)
  integer                        :: N_elec_in_key_hole_2(2),N_elec_in_key_part_2(2)
  
  double precision               :: mo_bielec_integral
  integer, allocatable           :: ia_ja_pairs(:,:,:)
  integer, allocatable           :: ib_jb_pairs(:,:)
  double precision               :: diag_H_mat_elem
  integer                        :: iproc
  integer(omp_lock_kind), save   :: lck, ifirst=0
  if (ifirst == 0) then
!$    call omp_init_lock(lck)
    ifirst=1
  endif
  
  $initialization
  
  iproc = 0
  $omp_parallel
  !$ iproc = omp_get_thread_num()
  allocate (keys_out(N_int,2,size_max), hole_save(N_int,2),          &
      key(N_int,2),hole(N_int,2), particle(N_int,2), hole_tmp(N_int,2),&
      particle_tmp(N_int,2), occ_particle(N_int*bit_kind_size,2),    &
      occ_hole(N_int*bit_kind_size,2), occ_particle_tmp(N_int*bit_kind_size,2),&
      occ_hole_tmp(N_int*bit_kind_size,2))
  $init_thread
  
  
  
  !!!! First couple hole particle
  do j = 1, N_int
    hole(j,1) = iand(hole_1(j,1),key_in(j,1))
    hole(j,2) = iand(hole_1(j,2),key_in(j,2))
    particle(j,1) = iand(xor(particl_1(j,1),key_in(j,1)),particl_1(j,1))
    particle(j,2) = iand(xor(particl_1(j,2),key_in(j,2)),particl_1(j,2))
  enddo
  call bitstring_to_list(particle(1,1),occ_particle(1,1),N_elec_in_key_part_1(1),N_int)
  call bitstring_to_list(particle(1,2),occ_particle(1,2),N_elec_in_key_part_1(2),N_int)
  call bitstring_to_list(hole(1,1),occ_hole(1,1),N_elec_in_key_hole_1(1),N_int)
  call bitstring_to_list(hole(1,2),occ_hole(1,2),N_elec_in_key_hole_1(2),N_int)
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
      
      call bitstring_to_list(particle_tmp(1,1),occ_particle_tmp(1,1),N_elec_in_key_part_2(1),N_int)
      call bitstring_to_list(particle_tmp(1,2),occ_particle_tmp(1,2),N_elec_in_key_part_2(2),N_int)
      call bitstring_to_list(hole_tmp    (1,1),occ_hole_tmp    (1,1),N_elec_in_key_hole_2(1),N_int)
      call bitstring_to_list(hole_tmp    (1,2),occ_hole_tmp    (1,2),N_elec_in_key_hole_2(2),N_int)
      
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
          do jjj=1,N_elec_in_key_part_2(other_spin)     ! particule
            j_b = occ_particle_tmp(jjj,other_spin)
            ASSERT (j_b > 0)
            ASSERT (j_b <= mo_tot_num)
            if (array_pairs(i_b,j_b)) then
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
  deallocate (ia_ja_pairs, ib_jb_pairs, &
      keys_out, hole_save,          &
      key,hole, particle, hole_tmp,&
      particle_tmp, occ_particle,    &
      occ_hole, occ_particle_tmp,&
      occ_hole_tmp,array_pairs)
  $omp_end_parallel
  $finalization
end

subroutine $subroutine_monoexc(key_in, hole_1,particl_1,i_generator $parameters )
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
  
  integer, allocatable           :: ia_ja_pairs(:,:,:)
  logical, allocatable           :: array_pairs(:,:)
  double precision               :: diag_H_mat_elem
  integer                        :: iproc
  integer(omp_lock_kind), save   :: lck, ifirst=0
  if (ifirst == 0) then
    ifirst=1
!$    call omp_init_lock(lck)
  endif
  
  $initialization
  
  iproc = 0
  $omp_parallel
  !$ iproc = omp_get_thread_num()
  allocate (keys_out(N_int,2,size_max), hole_save(N_int,2),          &
      key(N_int,2),hole(N_int,2), particle(N_int,2), hole_tmp(N_int,2),&
      particle_tmp(N_int,2), occ_particle(N_int*bit_kind_size,2),    &
      occ_hole(N_int*bit_kind_size,2), occ_particle_tmp(N_int*bit_kind_size,2),&
      occ_hole_tmp(N_int*bit_kind_size,2))
  $init_thread
  !!!! First couple hole particle
  do j = 1, N_int
    hole(j,1) = iand(hole_1(j,1),key_in(j,1))
    hole(j,2) = iand(hole_1(j,2),key_in(j,2))
    particle(j,1) = iand(xor(particl_1(j,1),key_in(j,1)),particl_1(j,1))
    particle(j,2) = iand(xor(particl_1(j,2),key_in(j,2)),particl_1(j,2))
  enddo
  
  call bitstring_to_list(particle(1,1),occ_particle(1,1),N_elec_in_key_part_1(1),N_int)
  call bitstring_to_list(particle(1,2),occ_particle(1,2),N_elec_in_key_part_1(2),N_int)
  call bitstring_to_list(hole    (1,1),occ_hole    (1,1),N_elec_in_key_hole_1(1),N_int)
  call bitstring_to_list(hole    (1,2),occ_hole    (1,2),N_elec_in_key_hole_1(2),N_int)
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
      hole(k,ispin) = ibclr(hole(k,ispin),j)
      k_a = ishft(j_a-1,-bit_kind_shift)+1
      l_a = j_a-ishft(k_a-1,bit_kind_shift)-1
      hole(k_a,ispin) = ibset(hole(k_a,ispin),l_a)
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
      occ_hole_tmp)
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
  double precision               :: wall_0, wall_1, wall_2
  integer(omp_lock_kind)         :: lck
  integer(bit_kind), allocatable :: mask(:,:,:)
  integer                        :: ispin, k

  PROVIDE H_apply_buffer_allocated mo_bielec_integrals_in_map N_det_selectors psi_generators
  PROVIDE psi_det_sorted_bit coef_hf_selector psi_det psi_coef H_apply_threshold
  
  nmax = ( N_det_generators/nproc ) *nproc
  call wall_time(wall_1)
  !$ call omp_init_lock(lck)
  !$OMP PARALLEL DEFAULT(SHARED) &
  !$OMP PRIVATE(i_generator,wall_2,ispin,k,mask) 
  allocate( mask(N_int,2,6) )
  !$OMP DO SCHEDULE(guided)
  do i_generator=1,nmax
    if (abort_here) then
      cycle
    endif
    $skip

    ! Create bit masks for holes and particles
    do ispin=1,2
      do k=1,N_int
        mask(k,ispin,s_hole) =                                      &
            iand(generators_bitmask(k,ispin,s_hole,i_bitmask_gen),  &
            psi_generators(k,ispin,i_generator) )
        mask(k,ispin,s_part) =                                      &
            iand(generators_bitmask(k,ispin,s_part,i_bitmask_gen),  &
            not(psi_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole1) =                                      &
            iand(generators_bitmask(k,ispin,d_hole1,i_bitmask_gen),  &
            psi_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part1) =                                      &
            iand(generators_bitmask(k,ispin,d_part1,i_bitmask_gen),  &
            not(psi_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole2) =                                      &
            iand(generators_bitmask(k,ispin,d_hole2,i_bitmask_gen),  &
            psi_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part2) =                                      &
            iand(generators_bitmask(k,ispin,d_part2,i_bitmask_gen),  &
            not (psi_generators(k,ispin,i_generator)) )
      enddo
    enddo

    call $subroutine_diexc(psi_generators(1,1,i_generator),          &
        mask(1,1,d_hole1), mask(1,1,d_part1),                        &
        mask(1,1,d_hole2), mask(1,1,d_part2),                        &
        i_generator $params_post)
    call $subroutine_monoexc(psi_generators(1,1,i_generator),        &
        mask(1,1,s_hole ), mask(1,1,s_part ),                        &
        i_generator $params_post)
    !$ call omp_set_lock(lck)
    call wall_time(wall_2)
    $printout_always
    if (wall_2 - wall_0 > 2.d0) then
        wall_0 = wall_2
        $printout_now
    endif
    !$ call omp_unset_lock(lck)
  enddo
  !$OMP END DO 
  deallocate( mask )
  !$OMP END PARALLEL
  !$ call omp_destroy_lock(lck)

  allocate( mask(N_int,2,6) )
  do i_generator=nmax+1,N_det_generators
    if (abort_here) then
      exit
    endif
    $skip

    ! Create bit masks for holes and particles
    do ispin=1,2
      do k=1,N_int
        mask(k,ispin,s_hole) =                                      &
            iand(generators_bitmask(k,ispin,s_hole,i_bitmask_gen),  &
            psi_generators(k,ispin,i_generator) )
        mask(k,ispin,s_part) =                                      &
            iand(generators_bitmask(k,ispin,s_part,i_bitmask_gen),  &
            not(psi_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole1) =                                      &
            iand(generators_bitmask(k,ispin,d_hole1,i_bitmask_gen),  &
            psi_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part1) =                                      &
            iand(generators_bitmask(k,ispin,d_part1,i_bitmask_gen),  &
            not(psi_generators(k,ispin,i_generator)) )
        mask(k,ispin,d_hole2) =                                      &
            iand(generators_bitmask(k,ispin,d_hole2,i_bitmask_gen),  &
            psi_generators(k,ispin,i_generator) )
        mask(k,ispin,d_part2) =                                      &
            iand(generators_bitmask(k,ispin,d_part2,i_bitmask_gen),  &
            not(psi_generators(k,ispin,i_generator)) )
      enddo
    enddo
    call $subroutine_diexc(psi_generators(1,1,i_generator),          &
        mask(1,1,d_hole1), mask(1,1,d_part1),                        &
        mask(1,1,d_hole2), mask(1,1,d_part2),                        &
        i_generator $params_post)
    call $subroutine_monoexc(psi_generators(1,1,i_generator),        &
        mask(1,1,s_hole ), mask(1,1,s_part ),                        &
        i_generator $params_post)
    call wall_time(wall_2)
    $printout_always
    if (wall_2 - wall_0 > 2.d0) then
        wall_0 = wall_2
        $printout_now
    endif
  enddo
  
  $copy_buffer
  $generate_psi_guess
  abort_here = abort_all
  deallocate( mask )
  
end

