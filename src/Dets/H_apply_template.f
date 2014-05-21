subroutine $subroutine_diexc(key_in, hole_1,particl_1, hole_2, particl_2 $parameters )
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
  integer(omp_lock_kind)         :: lck
  integer(bit_kind),intent(in)   :: key_in(N_int,2)
  integer(bit_kind),allocatable  :: keys_out(:,:,:)
  double precision, allocatable  :: hij_tab(:)
  integer(bit_kind), intent(in)  :: hole_1(N_int,2), particl_1(N_int,2)
  integer(bit_kind), intent(in)  :: hole_2(N_int,2), particl_2(N_int,2)
  integer(bit_kind)              :: hole_save(N_int,2)
  integer(bit_kind)              :: key(N_int,2),hole(N_int,2), particle(N_int,2)
  integer(bit_kind)              :: hole_tmp(N_int,2), particle_tmp(N_int,2)
  integer                        :: ii,i,jj,j,k,ispin,l
  integer                        :: occ_particle(N_int*bit_kind_size,2)
  integer                        :: occ_hole(N_int*bit_kind_size,2)
  integer                        :: occ_particle_tmp(N_int*bit_kind_size,2)
  integer                        :: occ_hole_tmp(N_int*bit_kind_size,2)
  integer                        :: kk,pp,other_spin,key_idx
  integer                        :: N_elec_in_key_hole_1(2),N_elec_in_key_part_1(2)
  integer                        :: N_elec_in_key_hole_2(2),N_elec_in_key_part_2(2)
  
  double precision               :: hij_elec, mo_bielec_integral, thresh
  integer, allocatable           :: ia_ja_pairs(:,:,:)
  double precision               :: diag_H_mat_elem
  
  PROVIDE mo_integrals_map ref_bitmask_energy key_pattern_not_in_ref
  PROVIDE mo_bielec_integrals_in_map reference_energy psi_ref_coef psi_ref
  
  $set_i_H_j_threshold
  
  $omp_init_lock
  
  
  $initialization
  
  $omp_parallel
  allocate (keys_out(N_int,2,size_max),hij_tab(size_max))
  
  !print*,'key_in !!'
  !call print_key(key_in)
  !print*,'hole_1, particl_1'
  !call print_key(hole_1)
  !call print_key(particl_1)
  !print*,'hole_2, particl_2'
  !call print_key(hole_2)
  !call print_key(particl_2)
  
  
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
  allocate (ia_ja_pairs(2,0:(elec_alpha_num)*mo_tot_num,2))
  
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
  accu = 0.d0
  hij_elec = 0.d0
  do ispin=1,2
    other_spin = iand(ispin,1)+1
    $omp_do
    do ii=1,ia_ja_pairs(1,0,ispin)
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
      if (ispin == 1) then
        integer                        :: jjj
        
        do kk = 1,N_elec_in_key_hole_2(other_spin)
          hole = hole_save
          i_b = occ_hole_tmp(kk,other_spin)
          ASSERT (i_b > 0)
          ASSERT (i_b <= mo_tot_num)
          k = ishft(i_b-1,-bit_kind_shift)+1
          j = i_b-ishft(k-1,bit_kind_shift)-1
          hole(k,other_spin) = ibclr(hole(k,other_spin),j)
          do jjj=1,N_elec_in_key_part_2(other_spin)     ! particule
            j_b = occ_particle_tmp(jjj,other_spin)
            ASSERT (j_b > 0)
            ASSERT (j_b <= mo_tot_num)
            if(dabs( mo_bielec_integral(j_a,j_b,i_a,i_b))<thresh)cycle
            key = hole
            k = ishft(j_b-1,-bit_kind_shift)+1
            l = j_b-ishft(k-1,bit_kind_shift)-1
            key(k,other_spin) = ibset(key(k,other_spin),l)
            call i_H_j(key,key_in,N_int,hij_elec)
            if(dabs(hij_elec)>=thresh) then
              key_idx += 1
              do k=1,N_int
                keys_out(k,1,key_idx) = key(k,1)
                keys_out(k,2,key_idx) = key(k,2)
              enddo
              hij_tab(key_idx) = hij_elec
              ASSERT (key_idx <= size_max)
              if (key_idx == size_max) then
                $keys_work_unlocked
                $omp_set_lock
                $keys_work_locked
                $omp_unset_lock
                key_idx = 0
              endif
            endif
          enddo
          if (key_idx > ishft(size_max,-5)) then
            if ($omp_test_lock) then
              $keys_work_unlocked
              $keys_work_locked
              $omp_unset_lock
              key_idx = 0
            endif
          endif
        enddo
      endif
      !   does all the mono excitations of the same spin
      do kk = 1,N_elec_in_key_hole_2(ispin)
        i_b = occ_hole_tmp(kk,ispin)
        ASSERT (i_b > 0)
        ASSERT (i_b <= mo_tot_num)
        if (i_b <= i_a.or.i_b == j_a) cycle
        hole = hole_save
        k = ishft(i_b-1,-bit_kind_shift)+1
        j = i_b-ishft(k-1,bit_kind_shift)-1
        hole(k,ispin) = ibclr(hole(k,ispin),j)
        do jjj=1,N_elec_in_key_part_2(ispin)
          j_b = occ_particle_tmp(jjj,ispin)
          ASSERT (j_b > 0)
          ASSERT (j_b <= mo_tot_num)
          if (j_b <= j_a) cycle
          if(dabs( mo_bielec_integral(j_a,j_b,i_b,i_a))<thresh)cycle
          key = hole
          k = ishft(j_b-1,-bit_kind_shift)+1
          l = j_b-ishft(k-1,bit_kind_shift)-1
          key(k,ispin) = ibset(key(k,ispin),l)
          !!   a^((+)_j_b(ispin) a_i_b(ispin) : mono exc :: orb(i_b,ispin) --> orb(j_b,ispin)
          
          call i_H_j(key,key_in,N_int,hij_elec)
          if(dabs(hij_elec)>=thresh) then
            key_idx += 1
            do k=1,N_int
              keys_out(k,1,key_idx) = key(k,1)
              keys_out(k,2,key_idx) = key(k,2)
            enddo
            hij_tab(key_idx) = hij_elec
            ASSERT (key_idx <= size_max)
            if (key_idx == size_max) then
              $keys_work_unlocked
              $omp_set_lock
              $keys_work_locked
              $omp_unset_lock
              key_idx = 0
            endif
          endif
        enddo
        if (key_idx > ishft(size_max,-5)) then
          if ($omp_test_lock) then
            $keys_work_locked
            $keys_work_unlocked
            $omp_unset_lock
            key_idx = 0
          endif
        endif
      enddo! kk
    enddo  ! ii
    $omp_enddo
  enddo   ! ispin
  $keys_work_unlocked
  $omp_set_lock
  $keys_work_locked
  $omp_unset_lock
  deallocate (keys_out,hij_tab,ia_ja_pairs)
  $omp_end_parallel
  $omp_destroy_lock
  $finalization
  
end

subroutine $subroutine_monoexc(key_in, hole_1,particl_1 $parameters )
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
  integer(omp_lock_kind)         :: lck
  integer(bit_kind),intent(in)   :: key_in(N_int,2)
  integer(bit_kind),allocatable  :: keys_out(:,:,:)
  double precision, allocatable  :: hij_tab(:)
  integer(bit_kind), intent(in)  :: hole_1(N_int,2), particl_1(N_int,2)
  integer(bit_kind)              :: hole_2(N_int,2), particl_2(N_int,2)
  integer(bit_kind)              :: hole_save(N_int,2)
  integer(bit_kind)              :: key(N_int,2),hole(N_int,2), particle(N_int,2)
  integer(bit_kind)              :: hole_tmp(N_int,2), particle_tmp(N_int,2)
  integer                        :: ii,i,jj,j,k,ispin,l
  integer                        :: occ_particle(N_int*bit_kind_size,2)
  integer                        :: occ_hole(N_int*bit_kind_size,2)
  integer                        :: occ_particle_tmp(N_int*bit_kind_size,2)
  integer                        :: occ_hole_tmp(N_int*bit_kind_size,2)
  integer                        :: kk,pp,other_spin,key_idx
  integer                        :: N_elec_in_key_hole_1(2),N_elec_in_key_part_1(2)
  integer                        :: N_elec_in_key_hole_2(2),N_elec_in_key_part_2(2)
  
  double precision               :: hij_elec, thresh
  integer, allocatable           :: ia_ja_pairs(:,:,:)
  double precision               :: diag_H_mat_elem
  
  PROVIDE mo_integrals_map ref_bitmask_energy key_pattern_not_in_ref
  PROVIDE mo_bielec_integrals_in_map reference_energy psi_ref_coef psi_ref

  $set_i_H_j_threshold
  
  $omp_init_lock
  
  
  $initialization
  
  $omp_parallel
  allocate (keys_out(N_int,2,size_max),hij_tab(size_max))
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
      hij_tab(key_idx) = hij_elec
      if (key_idx >  ishft(size_max,-5)) then
        if ($omp_test_lock) then
          $keys_work_unlocked
          $keys_work_locked
          $omp_unset_lock
          key_idx = 0
        endif
      endif
      if (key_idx == size_max) then
        $keys_work_unlocked
        $omp_set_lock
        $keys_work_locked
        $omp_unset_lock
        key_idx = 0
      endif
    enddo  ! ii
    $omp_enddo
  enddo   ! ispin
  $keys_work_unlocked
  $omp_set_lock
  $keys_work_locked
  $omp_unset_lock
  deallocate (keys_out,hij_tab,ia_ja_pairs)
  $omp_end_parallel
  $omp_destroy_lock
  $finalization
  
end

