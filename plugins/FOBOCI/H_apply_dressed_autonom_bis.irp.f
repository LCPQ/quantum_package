subroutine H_apply_dressed_pert_monoexc_bis(key_in, hole_1,particl_1,i_generator,iproc_in , delta_ij_generators_, Ndet_generators,psi_det_generators_input,E_ref,n_det_input,psi_det_input )
  use omp_lib
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Generate all single excitations of key_in using the bit masks of holes and
  ! particles.
  ! Assume N_int is already provided.
  END_DOC
  integer,parameter              :: size_max = 3072
  
  integer, intent(in) :: Ndet_generators,n_det_input
  double precision, intent(inout) :: delta_ij_generators_(n_det_input,n_det_input),E_ref
  
  integer(bit_kind), intent(in) :: psi_det_input(N_int,2,n_det_input)
  integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,Ndet_generators)

  integer          ,intent(in)   :: i_generator
  integer(bit_kind),intent(in)   :: key_in(N_int,2)
  integer(bit_kind),intent(in)   :: hole_1(N_int,2), particl_1(N_int,2)
  integer, intent(in)            :: iproc_in
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

  logical :: check_double_excitation 
  logical :: is_a_1h1p
  logical :: is_a_1h
  logical :: is_a_1p
  iproc = iproc_in

  check_double_excitation = .True.
  
     check_double_excitation = .False.
    



  if (ifirst == 0) then
    ifirst=1
!!$    call omp_init_lock(lck)
  endif
  
  
  
   PROVIDE elec_num_tab
!       !$OMP PARALLEL DEFAULT(SHARED)        &
!       !$OMP PRIVATE(i,j,k,l,keys_out,hole,particle,                &
!       !$OMP  occ_particle,occ_hole,j_a,k_a,other_spin,             &
!       !$OMP  hole_save,ispin,jj,l_a,ib_jb_pairs,array_pairs,       &
!       !$OMP  accu,i_a,hole_tmp,particle_tmp,occ_particle_tmp,      &
!       !$OMP  occ_hole_tmp,key_idx,i_b,j_b,key,N_elec_in_key_part_1,&
!       !$OMP  N_elec_in_key_hole_1,N_elec_in_key_part_2,            &
!       !$OMP  N_elec_in_key_hole_2,ia_ja_pairs,key_union_hole_part) &
!       !$OMP SHARED(key_in,N_int,elec_num_tab,mo_tot_num,           &
!       !$OMP  hole_1, particl_1, hole_2, particl_2,                 &
!       !$OMP  elec_alpha_num,i_generator) FIRSTPRIVATE(iproc)
!!$ iproc = omp_get_thread_num()
  allocate (keys_out(N_int,2,size_max), hole_save(N_int,2),          &
      key(N_int,2),hole(N_int,2), particle(N_int,2), hole_tmp(N_int,2),&
      particle_tmp(N_int,2), occ_particle(N_int*bit_kind_size,2),    &
      occ_hole(N_int*bit_kind_size,2), occ_particle_tmp(N_int*bit_kind_size,2),&
      occ_hole_tmp(N_int*bit_kind_size,2),key_union_hole_part(N_int))
  
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
  integer :: jjtest,na,nb
  do ispin=1,2
    other_spin = iand(ispin,1)+1
!   !$OMP DO SCHEDULE (static)
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
      na = 0
      nb = 0
!     if (is_a_1h(hole)) then    
!      cycle                     
!     endif                      
!     if (is_a_1p(hole)) then    
!      cycle                     
!     endif                      
      
      
      key_idx += 1
      do k=1,N_int
        keys_out(k,1,key_idx) = hole(k,1)
        keys_out(k,2,key_idx) = hole(k,2)
      enddo
      if (key_idx == size_max) then
        call standard_dress_bis(delta_ij_generators_,size_max,Ndet_generators,i_generator,key_idx,keys_out,N_int,iproc,psi_det_generators_input,E_ref,psi_det_input,n_det_input)
        key_idx = 0
      endif
    enddo  ! ii
!   !$OMP ENDDO NOWAIT
  enddo   ! ispin
  call standard_dress_bis(delta_ij_generators_,size_max,Ndet_generators,i_generator,key_idx,keys_out,N_int,iproc,psi_det_generators_input,E_ref,psi_det_input,n_det_input)
  
  deallocate (ia_ja_pairs, &
      keys_out, hole_save,          &
      key,hole, particle, hole_tmp,&
      particle_tmp, occ_particle,    &
      occ_hole, occ_particle_tmp,&
      occ_hole_tmp,key_union_hole_part)
! !$OMP END PARALLEL
  
  
end


subroutine H_apply_dressed_pertk_single(delta_ij_,  Ndet_generators,psi_det_generators_input,E_ref,psi_det_input,n_det_input)
  implicit none
  use omp_lib
  use bitmasks
  BEGIN_DOC
  ! Calls H_apply on the HF determinant and selects all connected single and double
  ! excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.
  END_DOC
  
  
    integer, intent(in) :: Ndet_generators,n_det_input
    integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,Ndet_generators)
    integer(bit_kind), intent(in) :: psi_det_input(N_int,2,n_det_input)
    double precision, intent(inout) :: delta_ij_(n_det_input,n_det_input),E_ref
    

  
  integer                        :: i_generator, nmax
  double precision               :: wall_0, wall_1
  integer(omp_lock_kind)         :: lck
  integer(bit_kind), allocatable :: mask(:,:,:)
  integer                        :: ispin, k
  integer                        :: iproc

  
  PROVIDE H_apply_buffer_allocated mo_bielec_integrals_in_map 
  
  nmax = mod( Ndet_generators,nproc )
 

! !$ call omp_init_lock(lck)
  call start_progress(Ndet_generators,'Selection (norm)',0.d0)

  call wall_time(wall_0)

  iproc = 0
  allocate( mask(N_int,2,6) )
  do i_generator=1,nmax

    progress_bar(1) = i_generator

    if (abort_here) then
      exit
    endif
    
    

!   ! Create bit masks for holes and particles
    do ispin=1,2
      do k=1,N_int
        mask(k,ispin,s_hole) =                                      &
            iand(generators_bitmask(k,ispin,s_hole,i_bitmask_gen),  &
            psi_det_generators_input(k,ispin,i_generator) )
        mask(k,ispin,s_part) =                                      &
            iand(generators_bitmask(k,ispin,s_part,i_bitmask_gen),  &
            not(psi_det_generators_input(k,ispin,i_generator)) )
        mask(k,ispin,d_hole1) =                                      &
            iand(generators_bitmask(k,ispin,d_hole1,i_bitmask_gen),  &
            psi_det_generators_input(k,ispin,i_generator) )
        mask(k,ispin,d_part1) =                                      &
            iand(generators_bitmask(k,ispin,d_part1,i_bitmask_gen),  &
            not(psi_det_generators_input(k,ispin,i_generator)) )
        mask(k,ispin,d_hole2) =                                      &
            iand(generators_bitmask(k,ispin,d_hole2,i_bitmask_gen),  &
            psi_det_generators_input(k,ispin,i_generator) )
        mask(k,ispin,d_part2) =                                      &
            iand(generators_bitmask(k,ispin,d_part2,i_bitmask_gen),  &
            not(psi_det_generators_input(k,ispin,i_generator)) )
      enddo
    enddo
     call H_apply_dressed_pert_monoexc(psi_det_generators_input(1,1,i_generator),    &
         mask(1,1,s_hole ), mask(1,1,s_part ),                        &
         i_generator, iproc , delta_ij_,  Ndet_generators,psi_det_generators_input,E_ref,n_det_input,psi_det_input)
    call wall_time(wall_1)
    
    if (wall_1 - wall_0 > 2.d0) then
        write(output_determinants,*)  &
       100.*float(i_generator)/float(Ndet_generators), '% in ', wall_1-wall_0, 's'
        wall_0 = wall_1
    endif
  enddo

  deallocate( mask )

! !$OMP PARALLEL DEFAULT(SHARED) &
! !$OMP PRIVATE(i_generator,wall_1,wall_0,ispin,k,mask,iproc) 
  call wall_time(wall_0)
! !$ iproc = omp_get_thread_num()
  allocate( mask(N_int,2,6) )
! !$OMP DO SCHEDULE(dynamic,1)
  do i_generator=nmax+1,Ndet_generators
    if (iproc == 0) then
      progress_bar(1) = i_generator
    endif
    if (abort_here) then
      cycle
    endif
    
    

    ! Create bit masks for holes and particles
    do ispin=1,2
      do k=1,N_int
        mask(k,ispin,s_hole) =                                      &
            iand(generators_bitmask(k,ispin,s_hole,i_bitmask_gen),  &
            psi_det_generators_input(k,ispin,i_generator) )
        mask(k,ispin,s_part) =                                      &
            iand(generators_bitmask(k,ispin,s_part,i_bitmask_gen),  &
            not(psi_det_generators_input(k,ispin,i_generator)) )
        mask(k,ispin,d_hole1) =                                      &
            iand(generators_bitmask(k,ispin,d_hole1,i_bitmask_gen),  &
            psi_det_generators_input(k,ispin,i_generator) )
        mask(k,ispin,d_part1) =                                      &
            iand(generators_bitmask(k,ispin,d_part1,i_bitmask_gen),  &
            not(psi_det_generators_input(k,ispin,i_generator)) )
        mask(k,ispin,d_hole2) =                                      &
            iand(generators_bitmask(k,ispin,d_hole2,i_bitmask_gen),  &
            psi_det_generators_input(k,ispin,i_generator) )
        mask(k,ispin,d_part2) =                                      &
            iand(generators_bitmask(k,ispin,d_part2,i_bitmask_gen),  &
            not (psi_det_generators_input(k,ispin,i_generator)) )
      enddo
    enddo

    if(.True.)then
      call H_apply_dressed_pert_monoexc(psi_det_generators_input(1,1,i_generator),  &
        mask(1,1,s_hole ), mask(1,1,s_part ),                        &
        i_generator, iproc , delta_ij_,  Ndet_generators,psi_det_generators_input,E_ref,n_det_input,psi_det_input)
    endif
!   !$ call omp_set_lock(lck)
    call wall_time(wall_1)
    
    if (wall_1 - wall_0 > 2.d0) then
        write(output_determinants,*)  &
       100.*float(i_generator)/float(Ndet_generators), '% in ', wall_1-wall_0, 's'
        wall_0 = wall_1
    endif
!   !$ call omp_unset_lock(lck)
  enddo
! !$OMP END DO 
  deallocate( mask )
! !$OMP END PARALLEL
! !$ call omp_destroy_lock(lck)

  abort_here = abort_all
  call stop_progress
  
  
  
  
end


subroutine standard_dress_bis(delta_ij_generators_,size_buffer,Ndet_generators,i_generator,n_selected,det_buffer,Nint,iproc,psi_det_generators_input,E_ref,psi_det_input,n_det_input)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc,n_det_input
  integer, intent(in) :: Ndet_generators,size_buffer
  double precision, intent(inout) :: delta_ij_generators_(n_det_input,n_det_input),E_ref

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,size_buffer)
  integer(bit_kind), intent(in)  :: psi_det_generators_input(N_int,2,Ndet_generators)
  integer(bit_kind), intent(in)  :: psi_det_input(N_int,2,n_det_input)
  integer                        :: i,j,k,m
  integer                        :: new_size
  integer                        :: degree(n_det_input)
  integer                        :: idx(0:n_det_input)
  logical                        :: good

  integer                        :: c_ref
  integer                        :: connected_to_ref


  double precision :: hka, haa
  double precision :: haj
  double precision :: f
  integer :: connected_to_ref_by_mono
  logical :: is_in_wavefunction
  double precision :: H_array(n_det_input)
  double precision :: contrib,lambda_i,accu
  integer ::  number_of_holes,n_h, number_of_particles,n_p

  do i=1,n_selected
       c_ref = connected_to_ref_by_mono(det_buffer(1,1,i),psi_det_generators_input,N_int,i_generator,Ndet_generators)
       if (c_ref /= 0) then
         cycle
       endif
       if (is_in_wavefunction(det_buffer(1,1,i),Nint)) then
         cycle
       endif
       print*
       n_h = number_of_holes(det_buffer(1,1,i))
       n_p = number_of_particles(det_buffer(1,1,i))
       print*,'n_h,n_p = ',n_h,n_p
       call get_excitation_degree_vector(psi_det_input,det_buffer(1,1,i),degree,N_int,n_det_input,idx)
       H_array = 0.d0
       do k=1,idx(0)
         call i_h_j(det_buffer(1,1,i),psi_det_input(1,1,idx(k)),Nint,hka)
         H_array(idx(k)) = hka
       enddo
         
       call i_h_j(det_buffer(1,1,i),det_buffer(1,1,i),Nint,haa)
       f = 1.d0/(E_ref-haa)
       
      lambda_i = f
     do k=1,idx(0)
       contrib = H_array(idx(k)) * H_array(idx(k)) * lambda_i
       delta_ij_generators_(idx(k), idx(k)) += contrib
       do j=k+1,idx(0)
         contrib = H_array(idx(k)) * H_array(idx(j)) * lambda_i
         delta_ij_generators_(idx(k), idx(j)) += contrib
         delta_ij_generators_(idx(j), idx(k)) += contrib
       enddo 
     enddo
  enddo
end

