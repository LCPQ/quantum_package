
subroutine FOBOCI_lmct_mlct_old_thr
  use bitmasks
 implicit none
 integer :: i,j,k,l
 integer(bit_kind),allocatable :: unpaired_bitmask(:,:)
 integer, allocatable :: occ(:,:)
 integer       :: n_occ_alpha, n_occ_beta
 double precision :: norm_tmp(N_states),norm_total(N_states)
 logical :: test_sym
 double precision :: thr,hij
 double precision :: threshold
 double precision, allocatable :: dressing_matrix(:,:)
 logical :: verbose,is_ok
 verbose = .True.
 threshold = threshold_singles
 print*,'threshold = ',threshold
 thr = 1.d-12
 allocate(unpaired_bitmask(N_int,2))
 allocate (occ(N_int*bit_kind_size,2))
 do i = 1, N_int
  unpaired_bitmask(i,1) = unpaired_alpha_electrons(i)
  unpaired_bitmask(i,2) = unpaired_alpha_electrons(i)
 enddo
 norm_total = 0.d0
 call initialize_density_matrix_osoci
 call bitstring_to_list(inact_bitmask(1,1), occ(1,1), n_occ_beta, N_int)
 print*,''
 print*,''
 print*,'mulliken spin population analysis'
 accu =0.d0
 do i = 1, nucl_num
  accu += mulliken_spin_densities(i)
  print*,i,nucl_charge(i),mulliken_spin_densities(i)
 enddo
 print*,''
 print*,''
 print*,'DOING FIRST LMCT !!'
 integer(bit_kind) , allocatable :: zero_bitmask(:,:)
 integer(bit_kind) , allocatable :: psi_singles(:,:,:)
 double precision, allocatable :: psi_singles_coef(:,:)
 allocate( zero_bitmask(N_int,2) )
  do i = 1, n_inact_orb
   integer :: i_hole_osoci
   i_hole_osoci = list_inact(i)
   print*,'--------------------------'
   ! First set the current generators to the one of restart
   call set_generators_to_generators_restart
   call set_psi_det_to_generators
   call check_symetry(i_hole_osoci,thr,test_sym)
   if(.not.test_sym)cycle
   print*,'i_hole_osoci = ',i_hole_osoci
   call create_restart_and_1h(i_hole_osoci)
   call set_generators_to_psi_det
   print*,'Passed set generators'
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_bitmask_hole_as_input(reunion_of_bitmask)
   call is_a_good_candidate(threshold,is_ok,verbose)
   print*,'is_ok = ',is_ok
   if(.not.is_ok)cycle
   allocate(dressing_matrix(N_det_generators,N_det_generators))
   dressing_matrix = 0.d0
   if(.not.do_it_perturbative)then

    do k = 1, N_det_generators
     do l = 1, N_det_generators
       call i_h_j(psi_det_generators(1,1,k),psi_det_generators(1,1,l),N_int,hkl)
       dressing_matrix(k,l) = hkl
     enddo
    enddo
    hkl = dressing_matrix(1,1)
    do k = 1, N_det_generators
      dressing_matrix(k,k) = dressing_matrix(k,k) - hkl
    enddo
    print*,'Naked matrix'
    do k = 1, N_det_generators
     write(*,'(100(F12.5,X))')dressing_matrix(k,:)
    enddo

    ! Do all the single excitations on top of the CAS and 1h determinants
    call set_bitmask_particl_as_input(reunion_of_bitmask)
    call set_bitmask_hole_as_input(reunion_of_bitmask)
    call all_single

!   ! Change the mask of the holes and particles to perform all the 
!   ! double excitations that starts from the active space in order 
!   ! to introduce the Coulomb hole in the active space
!   ! These are the 1h2p excitations that have the i_hole_osoci hole in common
!   ! and the 2p if there is more than one electron in the active space
!   do k = 1, N_int
!    zero_bitmask(k,1) = 0_bit_kind
!    zero_bitmask(k,2) = 0_bit_kind
!   enddo
!   ! hole is possible only in the orbital i_hole_osoci 
!   call set_bit_to_integer(i_hole_osoci,zero_bitmask(1,1),N_int)
!   call set_bit_to_integer(i_hole_osoci,zero_bitmask(1,2),N_int)
!   ! and in the active space
!   do k = 1, n_act_orb
!    call set_bit_to_integer(list_act(k),zero_bitmask(1,1),N_int)
!    call set_bit_to_integer(list_act(k),zero_bitmask(1,2),N_int)
!   enddo
!   call set_bitmask_hole_as_input(zero_bitmask)

!   call set_bitmask_particl_as_input(reunion_of_bitmask)

!   call all_1h2p
!   call diagonalize_CI_SC2
!   call provide_matrix_dressing(dressing_matrix,n_det_generators,psi_det_generators)

!   ! Change the mask of the holes and particles to perform all the 
!   ! double excitations that from the orbital i_hole_osoci
!   do k = 1, N_int
!    zero_bitmask(k,1) = 0_bit_kind
!    zero_bitmask(k,2) = 0_bit_kind
!   enddo
!   ! hole is possible only in the orbital i_hole_osoci 
!   call set_bit_to_integer(i_hole_osoci,zero_bitmask(1,1),N_int)
!   call set_bit_to_integer(i_hole_osoci,zero_bitmask(1,2),N_int)
!   call set_bitmask_hole_as_input(zero_bitmask)

!   call set_bitmask_particl_as_input(reunion_of_bitmask)

!   call set_psi_det_to_generators
!   call all_2h2p
!   call diagonalize_CI_SC2
    double precision :: hkl
    call provide_matrix_dressing(dressing_matrix,n_det_generators,psi_det_generators)
    hkl = dressing_matrix(1,1)
    do k = 1, N_det_generators
      dressing_matrix(k,k) = dressing_matrix(k,k) - hkl
    enddo
    print*,'Dressed matrix'
    do k = 1, N_det_generators
     write(*,'(100(F12.5,X))')dressing_matrix(k,:)
    enddo
!   call diag_dressed_matrix_and_set_to_psi_det(psi_det_generators,N_det_generators,dressing_matrix)
   endif
   call set_intermediate_normalization_lmct_old(norm_tmp,i_hole_osoci)

   do k = 1, N_states
    print*,'norm_tmp = ',norm_tmp(k)
    norm_total(k) += norm_tmp(k)
   enddo
   call update_density_matrix_osoci
   deallocate(dressing_matrix)
 enddo

 if(.True.)then
  print*,''
  print*,'DOING THEN THE MLCT !!'
   do i = 1, n_virt_orb
    integer :: i_particl_osoci
    i_particl_osoci = list_virt(i)
    print*,'--------------------------'
    ! First set the current generators to the one of restart
    call set_generators_to_generators_restart
    call set_psi_det_to_generators
    call check_symetry(i_particl_osoci,thr,test_sym)
    if(.not.test_sym)cycle
    print*,'i_particl_osoci= ',i_particl_osoci
    ! Initialize the bitmask to the restart ones
    call initialize_bitmask_to_restart_ones
    ! Impose that only the hole i_hole_osoci can be done
    call modify_bitmasks_for_particl(i_particl_osoci)
    call print_generators_bitmasks_holes
    ! Impose that only the active part can be reached 
    call set_bitmask_hole_as_input(unpaired_bitmask)
!!  call all_single_h_core
    call create_restart_and_1p(i_particl_osoci)
!!  ! Update the generators 
    call set_generators_to_psi_det
    call set_bitmask_particl_as_input(reunion_of_bitmask)
    call set_bitmask_hole_as_input(reunion_of_bitmask)
!!  ! so all the mono excitation on the new generators 
    call is_a_good_candidate(threshold,is_ok,verbose)
    print*,'is_ok = ',is_ok
    if(.not.is_ok)cycle
     allocate(dressing_matrix(N_det_generators,N_det_generators))
    if(.not.do_it_perturbative)then
     dressing_matrix = 0.d0
     do k = 1, N_det_generators
      do l = 1, N_det_generators
        call i_h_j(psi_det_generators(1,1,k),psi_det_generators(1,1,l),N_int,hkl)
        dressing_matrix(k,l) = hkl
      enddo
     enddo
 !   call all_single_split(psi_det_generators,psi_coef_generators,N_det_generators,dressing_matrix)
 !   call diag_dressed_matrix_and_set_to_psi_det(psi_det_generators,N_det_generators,dressing_matrix)
     call all_single
    endif
    call set_intermediate_normalization_mlct_old(norm_tmp,i_particl_osoci)
    do k = 1, N_states
     print*,'norm_tmp = ',norm_tmp(k)
     norm_total(k) += norm_tmp(k)
    enddo
    call update_density_matrix_osoci
   deallocate(dressing_matrix)
  enddo
 endif

   print*,'norm_total = ',norm_total
   norm_total = norm_generators_restart
   norm_total = 1.d0/norm_total
!  call rescale_density_matrix_osoci(norm_total)
   double precision :: accu
   accu = 0.d0
   do i = 1, mo_tot_num
    accu += one_body_dm_mo_alpha_osoci(i,i) + one_body_dm_mo_beta_osoci(i,i)
   enddo
   print*,'accu = ',accu
end


subroutine FOBOCI_mlct_old
  use bitmasks
 implicit none
 integer :: i,j,k,l
 integer(bit_kind),allocatable :: unpaired_bitmask(:,:)
 integer, allocatable :: occ(:,:)
 integer       :: n_occ_alpha, n_occ_beta
 double precision :: norm_tmp,norm_total
 logical :: test_sym
 double precision :: thr
 double precision :: threshold
 logical :: verbose,is_ok
 verbose = .False.
 threshold = 1.d-2
 thr = 1.d-12
 allocate(unpaired_bitmask(N_int,2))
 allocate (occ(N_int*bit_kind_size,2))
 do i = 1, N_int
  unpaired_bitmask(i,1) = unpaired_alpha_electrons(i)
  unpaired_bitmask(i,2) = unpaired_alpha_electrons(i)
 enddo
 norm_total = 0.d0
 call initialize_density_matrix_osoci
 call bitstring_to_list(inact_bitmask(1,1), occ(1,1), n_occ_beta, N_int)
 print*,''
 print*,''
 print*,''
 print*,'DOING FIRST MLCT !!'
  do i = 1, n_virt_orb
   integer :: i_particl_osoci
   i_particl_osoci = list_virt(i)
   print*,'--------------------------'
   ! First set the current generators to the one of restart
   call set_generators_to_generators_restart
   call set_psi_det_to_generators
   call check_symetry(i_particl_osoci,thr,test_sym)
   if(.not.test_sym)cycle
   print*,'i_particl_osoci= ',i_particl_osoci
   ! Initialize the bitmask to the restart ones
   call initialize_bitmask_to_restart_ones
   ! Impose that only the hole i_hole_osoci can be done
   call modify_bitmasks_for_particl(i_particl_osoci)
   call print_generators_bitmasks_holes
   ! Impose that only the active part can be reached 
   call set_bitmask_hole_as_input(unpaired_bitmask)
!  call all_single_h_core
   call create_restart_and_1p(i_particl_osoci)
!  ! Update the generators 
   call set_generators_to_psi_det
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_bitmask_hole_as_input(reunion_of_bitmask)
!  ! so all the mono excitation on the new generators 
   call is_a_good_candidate(threshold,is_ok,verbose)
   print*,'is_ok = ',is_ok
   is_ok =.True.
   if(.not.is_ok)cycle
   call all_single
   call set_intermediate_normalization_mlct_old(norm_tmp,i_particl_osoci)
   print*,'norm_tmp = ',norm_tmp
   norm_total += norm_tmp
   call update_density_matrix_osoci
 enddo

 print*,'norm_total = ',norm_total
 norm_total += 1.d0 
 norm_total = 1.d0/norm_total
 call rescale_density_matrix_osoci(norm_total)
 double precision :: accu
 accu = 0.d0
 do i = 1, mo_tot_num
  accu += one_body_dm_mo_alpha_osoci(i,i) + one_body_dm_mo_beta_osoci(i,i)
 enddo
 print*,'accu = ',accu
end


subroutine FOBOCI_lmct_old
  use bitmasks
 implicit none
 integer :: i,j,k,l
 integer(bit_kind),allocatable :: unpaired_bitmask(:,:)
 integer, allocatable :: occ(:,:)
 integer       :: n_occ_alpha, n_occ_beta
 double precision :: norm_tmp,norm_total
 logical :: test_sym
 double precision :: thr
 double precision :: threshold
 logical :: verbose,is_ok
 verbose = .False.
 threshold = 1.d-2
 thr = 1.d-12
 allocate(unpaired_bitmask(N_int,2))
 allocate (occ(N_int*bit_kind_size,2))
 do i = 1, N_int
  unpaired_bitmask(i,1) = unpaired_alpha_electrons(i)
  unpaired_bitmask(i,2) = unpaired_alpha_electrons(i)
 enddo
 norm_total = 0.d0
 call initialize_density_matrix_osoci
 call bitstring_to_list(inact_bitmask(1,1), occ(1,1), n_occ_beta, N_int)
 print*,''
 print*,''
 print*,'DOING FIRST LMCT !!'
  do i = 1, n_inact_orb
   integer :: i_hole_osoci
   i_hole_osoci = list_inact(i)
   print*,'--------------------------'
   ! First set the current generators to the one of restart
   call set_generators_to_generators_restart
   call set_psi_det_to_generators
   call check_symetry(i_hole_osoci,thr,test_sym)
   if(.not.test_sym)cycle
   print*,'i_hole_osoci = ',i_hole_osoci
   ! Initialize the bitmask to the restart ones
   call initialize_bitmask_to_restart_ones
   ! Impose that only the hole i_hole_osoci can be done
   call modify_bitmasks_for_hole(i_hole_osoci)
   call print_generators_bitmasks_holes
   ! Impose that only the active part can be reached 
   call set_bitmask_particl_as_input(unpaired_bitmask)
!  call all_single_h_core
   call create_restart_and_1h(i_hole_osoci)
!  ! Update the generators 
   call set_generators_to_psi_det
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_bitmask_hole_as_input(reunion_of_bitmask)
   call is_a_good_candidate(threshold,is_ok,verbose)
   print*,'is_ok = ',is_ok
   if(.not.is_ok)cycle
!  ! so all the mono excitation on the new generators 
   call all_single
!  call set_intermediate_normalization_lmct_bis(norm_tmp,i_hole_osoci)
   call set_intermediate_normalization_lmct_old(norm_tmp,i_hole_osoci)
   print*,'norm_tmp = ',norm_tmp
   norm_total += norm_tmp
   call update_density_matrix_osoci
 enddo

   print*,'norm_total = ',norm_total
   norm_total += 1.d0 
   norm_total = 1.d0/norm_total
   call rescale_density_matrix_osoci(norm_total)
   double precision :: accu
   accu = 0.d0
   do i = 1, mo_tot_num
    accu += one_body_dm_mo_alpha_osoci(i,i) + one_body_dm_mo_beta_osoci(i,i)
   enddo
   print*,'accu = ',accu
end
