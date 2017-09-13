
subroutine new_approach
  use bitmasks
 implicit none
 integer :: n_max_good_det
 n_max_good_det = n_inact_orb * n_act_orb *n_det_generators_restart + n_virt_orb * n_act_orb * n_det_generators_restart
 integer :: n_good_det,n_good_hole, n_good_particl
 n_good_det = 0
 n_good_hole = 0
 n_good_particl = 0
 integer(bit_kind), allocatable :: psi_good_det(:,:,:)
 double precision, allocatable :: dressing_restart_good_det(:,:)
 double precision, allocatable :: dressing_matrix_restart_1h1p(:,:)
 double precision, allocatable :: dressing_matrix_restart_2h1p(:,:)
 double precision, allocatable :: dressing_matrix_restart_1h2p(:,:)
 double precision, allocatable :: dressing_diag_good_det(:)

 double precision :: hjk

 integer :: i,j,k,l,i_hole_foboci
 logical :: test_sym
 double precision :: thr,hij
 double precision :: threshold,accu
 double precision, allocatable :: dressing_matrix_1h1p(:,:)
 double precision, allocatable :: dressing_matrix_2h1p(:,:)
 double precision, allocatable :: dressing_matrix_1h2p(:,:)
 double precision, allocatable :: dressing_matrix_extra_1h_or_1p(:,:)
 double precision, allocatable :: H_matrix_tmp(:,:)
 logical :: verbose,is_ok

 double precision,allocatable :: eigenvectors(:,:), eigenvalues(:)


 allocate(psi_good_det(N_int,2,n_max_good_det))
 allocate(dressing_restart_good_det(n_max_good_det,n_det_generators_restart))
 allocate(dressing_matrix_restart_1h1p(N_det_generators_restart, N_det_generators_restart))
 allocate(dressing_matrix_restart_2h1p(N_det_generators_restart, N_det_generators_restart))
 allocate(dressing_matrix_restart_1h2p(N_det_generators_restart, N_det_generators_restart))
 allocate(dressing_diag_good_det(n_max_good_det))

 dressing_restart_good_det = 0.d0
 dressing_matrix_restart_1h1p = 0.d0
 dressing_matrix_restart_2h1p = 0.d0
 dressing_matrix_restart_1h2p = 0.d0
 dressing_diag_good_det = 0.d0


 verbose = .True.
 threshold = threshold_lmct
 print*,'threshold = ',threshold
 thr = 1.d-12
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
 integer :: i_particl_osoci

 do i = 1, n_inact_orb
   i_hole_foboci = list_inact(i)
   print*,'--------------------------'
   ! First set the current generators to the one of restart
   call set_generators_to_generators_restart
   call set_psi_det_to_generators
   call check_symetry(i_hole_foboci,thr,test_sym)
   if(.not.test_sym)cycle
   print*,'i_hole_foboci = ',i_hole_foboci
   call create_restart_and_1h(i_hole_foboci)
!  ! Update the generators 
   call set_generators_to_psi_det
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_bitmask_hole_as_input(reunion_of_bitmask)
   call is_a_good_candidate(threshold,is_ok,verbose)
   print*,'is_ok = ',is_ok
   if(.not.is_ok)cycle
   ! so all the mono excitation on the new generators 
   allocate(dressing_matrix_1h1p(N_det_generators,N_det_generators))
   allocate(dressing_matrix_2h1p(N_det_generators,N_det_generators))
   allocate(dressing_matrix_extra_1h_or_1p(N_det_generators,N_det_generators))
   dressing_matrix_1h1p = 0.d0
   dressing_matrix_2h1p = 0.d0
   dressing_matrix_extra_1h_or_1p = 0.d0
   if(.not.do_it_perturbative)then
    n_good_hole +=1
!   call all_single_split_for_1h(dressing_matrix_1h1p,dressing_matrix_2h1p)
    call all_single_for_1h(i_hole_foboci,dressing_matrix_1h1p,dressing_matrix_2h1p,dressing_matrix_extra_1h_or_1p)
    allocate(H_matrix_tmp(N_det_generators,N_det_generators))
    do j = 1,N_det_generators
     do k = 1, N_det_generators
      call i_h_j(psi_det_generators(1,1,j),psi_det_generators(1,1,k),N_int,hjk)
      H_matrix_tmp(j,k) = hjk
     enddo
    enddo
    do j = 1, N_det_generators
     do k = 1, N_det_generators
      H_matrix_tmp(j,k) += dressing_matrix_1h1p(j,k) + dressing_matrix_2h1p(j,k) + dressing_matrix_extra_1h_or_1p(j,k)
     enddo
    enddo
    hjk = H_matrix_tmp(1,1)
    do j = 1, N_det_generators
      H_matrix_tmp(j,j) -= hjk
    enddo
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'Dressed matrix :'
    do j = 1, N_det_generators
     write(*,'(100(X,F8.5))') H_matrix_tmp(j,:)
    enddo
    allocate(eigenvectors(N_det_generators,N_det_generators), eigenvalues(N_det_generators))
    call lapack_diag(eigenvalues,eigenvectors,H_matrix_tmp,N_det_generators,N_det_generators)
    print*,'Eigenvector of the dressed matrix :'
    do j = 1, N_det_generators
     print*,'coef = ',eigenvectors(j,1)
    enddo
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    deallocate(eigenvectors, eigenvalues)
    deallocate(H_matrix_tmp)
    call update_dressing_matrix(dressing_matrix_1h1p,dressing_matrix_2h1p,dressing_restart_good_det,dressing_matrix_restart_1h1p, &
                                  dressing_matrix_restart_2h1p,dressing_diag_good_det,psi_good_det,n_good_det,n_max_good_det)
   endif
   deallocate(dressing_matrix_1h1p)
   deallocate(dressing_matrix_2h1p)
   deallocate(dressing_matrix_extra_1h_or_1p)
 enddo
 
 print*,''
 print*,''
 print*,'DOING THEN THE MLCT !!'
 do i = 1, n_virt_orb
   i_particl_osoci = list_virt(i)
   print*,'--------------------------'
   ! First set the current generators to the one of restart
   call set_generators_to_generators_restart
   call set_psi_det_to_generators
   call check_symetry(i_particl_osoci,thr,test_sym)
   if(.not.test_sym)cycle
   print*,'i_part_foboci = ',i_particl_osoci
   call create_restart_and_1p(i_particl_osoci)
   ! Update the generators 
   call set_generators_to_psi_det
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_bitmask_hole_as_input(reunion_of_bitmask)
   call is_a_good_candidate(threshold,is_ok,verbose)
   print*,'is_ok = ',is_ok
   if(.not.is_ok)cycle
   ! so all the mono excitation on the new generators 
   allocate(dressing_matrix_1h1p(N_det_generators,N_det_generators))
   allocate(dressing_matrix_1h2p(N_det_generators,N_det_generators))
   allocate(dressing_matrix_extra_1h_or_1p(N_det_generators,N_det_generators))
   dressing_matrix_1h1p = 0.d0
   dressing_matrix_1h2p = 0.d0
   dressing_matrix_extra_1h_or_1p = 0.d0
   if(.not.do_it_perturbative)then
    n_good_hole +=1
!   call all_single_split_for_1p(dressing_matrix_1h1p,dressing_matrix_1h2p)
    call all_single_for_1p(i_particl_osoci,dressing_matrix_1h1p,dressing_matrix_1h2p,dressing_matrix_extra_1h_or_1p)
    allocate(H_matrix_tmp(N_det_generators,N_det_generators))
    do j = 1,N_det_generators
     do k = 1, N_det_generators
      call i_h_j(psi_det_generators(1,1,j),psi_det_generators(1,1,k),N_int,hjk)
      H_matrix_tmp(j,k) = hjk
     enddo
    enddo
    do j = 1, N_det_generators
     do k = 1, N_det_generators
      H_matrix_tmp(j,k) += dressing_matrix_1h1p(j,k) + dressing_matrix_1h2p(j,k) + dressing_matrix_extra_1h_or_1p(j,k)
     enddo
    enddo
    hjk = H_matrix_tmp(1,1)
    do j = 1, N_det_generators
      H_matrix_tmp(j,j) -= hjk
    enddo
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'Dressed matrix :'
    do j = 1, N_det_generators
     write(*,'(100(F8.5))') H_matrix_tmp(j,:)
    enddo
    allocate(eigenvectors(N_det_generators,N_det_generators), eigenvalues(N_det_generators))
    call lapack_diag(eigenvalues,eigenvectors,H_matrix_tmp,N_det_generators,N_det_generators)
    print*,'Eigenvector of the dressed matrix :'
    do j = 1, N_det_generators
     print*,'coef = ',eigenvectors(j,1)
    enddo
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    print*,'-----------------------'
    deallocate(eigenvectors, eigenvalues)
    deallocate(H_matrix_tmp)
    call update_dressing_matrix(dressing_matrix_1h1p,dressing_matrix_1h2p,dressing_restart_good_det,dressing_matrix_restart_1h1p, &
                                  dressing_matrix_restart_1h2p,dressing_diag_good_det,psi_good_det,n_good_det,n_max_good_det)

   endif
   deallocate(dressing_matrix_1h1p)
   deallocate(dressing_matrix_1h2p)
   deallocate(dressing_matrix_extra_1h_or_1p)
 enddo


 double precision, allocatable :: H_matrix_total(:,:)
 integer :: n_det_total
 n_det_total = N_det_generators_restart + n_good_det
 allocate(H_matrix_total(n_det_total, n_det_total))
 ! Building of the effective Hamiltonian   
 ! We assume that the first determinants are the n_det_generators_restart ones
 ! and then come the n_good_det determinants in psi_good_det
 H_matrix_total = 0.d0
 do i = 1, N_det_generators_restart
  do j = 1, N_det_generators_restart
   call i_H_j(psi_det_generators_restart(1,1,i),psi_det_generators_restart(1,1,j),N_int,hij)
   H_matrix_total(i,j) = hij
   !!! Adding the averaged dressing coming from the 1h1p that are redundant for each of the "n_good_hole" 1h
   H_matrix_total(i,j) += dressing_matrix_restart_1h1p(i,j)/dble(n_good_hole+n_good_particl)  
   !!! Adding the dressing coming from the 2h1p that are not redundant for the any of CI calculations
   H_matrix_total(i,j) += dressing_matrix_restart_2h1p(i,j) + dressing_matrix_restart_1h2p(i,j)
  enddo
 enddo
 do i = 1, n_good_det
  call i_H_j(psi_good_det(1,1,i),psi_good_det(1,1,i),N_int,hij)
  !!! Adding the diagonal dressing coming from the singles 
  H_matrix_total(n_det_generators_restart+i,n_det_generators_restart+i) = hij + dressing_diag_good_det(i) 
  do j = 1, N_det_generators_restart
   !!! Adding the extra diagonal dressing between the references and the singles
   print*,' dressing_restart_good_det = ',dressing_restart_good_det(i,j)
   call i_H_j(psi_good_det(1,1,i),psi_det_generators_restart(1,1,j),N_int,hij)
   H_matrix_total(n_det_generators_restart+i,j) += hij
   H_matrix_total(j,n_det_generators_restart+i) += hij
   H_matrix_total(j,n_det_generators_restart+i) += dressing_restart_good_det(i,j)
   H_matrix_total(n_det_generators_restart+i,j) += dressing_restart_good_det(i,j)
  enddo
  do j = i+1, n_good_det
   !!! Building the naked Hamiltonian matrix between the singles 
   call i_H_j(psi_good_det(1,1,i),psi_good_det(1,1,j),N_int,hij)
   H_matrix_total(n_det_generators_restart+i,n_det_generators_restart+j) = hij 
   H_matrix_total(n_det_generators_restart+j,n_det_generators_restart+i) = hij 
  enddo
 enddo

 ! Adding the correlation energy 
 logical :: orb_taken_good_det(mo_tot_num)
 double precision :: phase
 integer :: n_h,n_p,number_of_holes,number_of_particles
 integer :: exc(0:2,2,2)
 integer :: degree
 integer :: h1,h2,p1,p2,s1,s2
 logical, allocatable :: one_hole_or_one_p(:)
 integer, allocatable :: holes_or_particle(:)
 allocate(one_hole_or_one_p(n_good_det), holes_or_particle(n_good_det))
 orb_taken_good_det = .False.
 do i = 1, n_good_det
  n_h = number_of_holes(psi_good_det(1,1,i))
  n_p = number_of_particles(psi_good_det(1,1,i))
  call get_excitation(ref_bitmask,psi_good_det(1,1,i),exc,degree,phase,N_int)
  call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
  if(n_h == 0 .and. n_p == 1)then
   orb_taken_good_det(h1) = .True.
   one_hole_or_one_p(i) = .True.
   holes_or_particle(i) = h1
  endif
  if(n_h == 1 .and. n_p == 0)then
   orb_taken_good_det(p1) = .True.
   one_hole_or_one_p(i) = .False.
   holes_or_particle(i) = p1
  endif
 enddo
 
  do i = 1, N_det_generators_restart
   ! Add the 2h2p, 2h1p and 1h2p correlation energy
   H_matrix_total(i,i) += total_corr_e_2h2p + total_corr_e_2h1p + total_corr_e_1h2p + total_corr_e_1h1p_spin_flip
   ! Substract the 2h1p part that have already been taken into account
   do j = 1, n_inact_orb
    iorb = list_inact(j)
    if(.not.orb_taken_good_det(iorb))cycle
    H_matrix_total(i,i) -= corr_energy_2h1p_per_orb_ab(iorb) - corr_energy_2h1p_per_orb_bb(iorb) - corr_energy_1h1p_spin_flip_per_orb(iorb)
   enddo
   ! Substract the 1h2p part that have already been taken into account
   do j = 1, n_virt_orb
    iorb = list_virt(j)
    if(.not.orb_taken_good_det(iorb))cycle
    H_matrix_total(i,i) -= corr_energy_1h2p_per_orb_ab(iorb) - corr_energy_1h2p_per_orb_aa(iorb)
   enddo
  enddo
  
  do i = 1, N_good_det
   ! Repeat the 2h2p correlation energy
   H_matrix_total(N_det_generators_restart+i,N_det_generators_restart+i) += total_corr_e_2h2p 
   ! Substract the part that can not be repeated
   ! If it is a 1h
   if(one_hole_or_one_p(i))then
    ! 2h2p 
    H_matrix_total(N_det_generators_restart+i,N_det_generators_restart+i) += -corr_energy_2h2p_per_orb_ab(holes_or_particle(i)) &  
                                                                             -corr_energy_2h2p_per_orb_bb(holes_or_particle(i))  
    ! You can repeat a certain part of the 1h2p correlation energy
    ! that is everything except the part that involves the hole of the 1h
    H_matrix_total(N_det_generators_restart+i,N_det_generators_restart+i) += total_corr_e_1h2p 
    H_matrix_total(N_det_generators_restart+i,N_det_generators_restart+i) += -corr_energy_1h2p_per_orb_ab(holes_or_particle(i)) &  
                                                                             -corr_energy_1h2p_per_orb_bb(holes_or_particle(i))  
   
   else
    ! 2h2p 
    H_matrix_total(N_det_generators_restart+i,N_det_generators_restart+i) += -corr_energy_2h2p_per_orb_ab(holes_or_particle(i)) &  
                                                                             -corr_energy_2h2p_per_orb_aa(holes_or_particle(i))  
    ! You can repeat a certain part of the 2h1p correlation energy
    ! that is everything except the part that involves the hole of the 1p
    ! 2h1p
    H_matrix_total(N_det_generators_restart+i,N_det_generators_restart+i) += -corr_energy_2h1p_per_orb_ab(holes_or_particle(i)) &  
                                                                             -corr_energy_2h1p_per_orb_aa(holes_or_particle(i))  
   endif
  enddo

 allocate(psi_coef_final(n_det_total, N_states))
 allocate(psi_det_final(N_int,2,n_det_total))
 do i = 1, N_det_generators_restart
  do j = 1,N_int 
   psi_det_final(j,1,i) = psi_det_generators_restart(j,1,i)
   psi_det_final(j,2,i) = psi_det_generators_restart(j,2,i)
  enddo
 enddo
 do i = 1, n_good_det
  do j = 1,N_int 
   psi_det_final(j,1,n_det_generators_restart+i) = psi_good_det(j,1,i)
   psi_det_final(j,2,n_det_generators_restart+i) = psi_good_det(j,2,i)
  enddo
 enddo


 double precision :: href
 double precision, allocatable :: eigvalues(:),eigvectors(:,:)
 integer(bit_kind), allocatable :: psi_det_final(:,:,:)
 double precision, allocatable :: psi_coef_final(:,:)
 double precision :: norm
 allocate(eigvalues(n_det_total),eigvectors(n_det_total,n_det_total))

 call lapack_diag(eigvalues,eigvectors,H_matrix_total,n_det_total,n_det_total)
 print*,''
 print*,''
 print*,'H_matrix_total(1,1) = ',H_matrix_total(1,1)
 print*,'e_dressed  = ',eigvalues(1) + nuclear_repulsion
 do i = 1, n_det_total
  print*,'coef = ',eigvectors(i,1),H_matrix_total(i,i) - H_matrix_total(1,1)
 enddo

 integer(bit_kind), allocatable :: psi_det_remaining_1h_or_1p(:,:,:)
 integer(bit_kind), allocatable :: key_tmp(:,:)
 integer :: n_det_remaining_1h_or_1p
 integer :: ispin,i_ok
 allocate(key_tmp(N_int,2),psi_det_remaining_1h_or_1p(N_int,2,n_inact_orb*n_act_orb+n_virt_orb*n_act_orb))
 logical :: is_already_present
 logical, allocatable :: one_hole_or_one_p_bis(:)
 integer, allocatable :: holes_or_particle_bis(:)
 double precision,allocatable :: H_array(:)
 allocate(one_hole_or_one_p_bis(n_inact_orb*n_act_orb+n_virt_orb*n_act_orb), holes_or_particle_bis(n_inact_orb*n_act_orb+n_virt_orb*n_act_orb))
 allocate(H_array(n_det_total))
 ! Dressing with the remaining 1h determinants
 print*,''
 print*,''
 print*,'Dressing with the remaining 1h determinants'
 n_det_remaining_1h_or_1p = 0
 do i = 1, n_inact_orb
  iorb = list_inact(i)
  if(orb_taken_good_det(iorb))cycle
  do j = 1, n_act_orb
   jorb = list_act(j)
   ispin = 2
   key_tmp = ref_bitmask
   call do_mono_excitation(key_tmp,iorb,jorb,ispin,i_ok)
   if(i_ok .ne.1)cycle
   is_already_present = .False.
   H_array = 0.d0
   call i_h_j(key_tmp,key_tmp,N_int,hij)
   href = ref_bitmask_energy - hij
   href = 1.d0/href
   do k = 1, n_det_total
    call get_excitation_degree(psi_det_final(1,1,k),key_tmp,degree,N_int)
    if(degree == 0)then
     is_already_present = .True.
     exit
    endif
   enddo
   if(is_already_present)cycle
   n_det_remaining_1h_or_1p +=1
   one_hole_or_one_p_bis(n_det_remaining_1h_or_1p) = .True.
   holes_or_particle_bis(n_det_remaining_1h_or_1p) = iorb
   do k = 1, N_int
    psi_det_remaining_1h_or_1p(k,1,n_det_remaining_1h_or_1p) = key_tmp(k,1)
    psi_det_remaining_1h_or_1p(k,2,n_det_remaining_1h_or_1p) = key_tmp(k,2)
   enddo
 ! do k = 1, n_det_total
 !   call i_h_j(psi_det_final(1,1,k),key_tmp,N_int,hij)
 !   H_array(k) = hij
 ! enddo
 ! do k = 1, n_det_total 
 !  do l = 1, n_det_total
 !   H_matrix_total(k,l) += H_array(k) * H_array(l) * href
 !  enddo
 ! enddo
  enddo
 enddo
 ! Dressing with the remaining 1p determinants
 print*,'n_det_remaining_1h_or_1p = ',n_det_remaining_1h_or_1p
 print*,'Dressing with the remaining 1p determinants'
 do i = 1, n_virt_orb
  iorb = list_virt(i)
  if(orb_taken_good_det(iorb))cycle
  do j = 1, n_act_orb
   jorb = list_act(j)
   ispin = 1
   key_tmp = ref_bitmask
   call do_mono_excitation(key_tmp,jorb,iorb,ispin,i_ok)
   if(i_ok .ne.1)cycle
   is_already_present = .False.
   H_array = 0.d0
   call i_h_j(key_tmp,key_tmp,N_int,hij)
   href = ref_bitmask_energy - hij
   href = 1.d0/href
   do k = 1, n_det_total
    call get_excitation_degree(psi_det_final(1,1,k),key_tmp,degree,N_int)
    if(degree == 0)then
     is_already_present = .True.
     exit
    endif
   enddo
   if(is_already_present)cycle
   n_det_remaining_1h_or_1p +=1
   one_hole_or_one_p_bis(n_det_remaining_1h_or_1p) = .False.
   holes_or_particle_bis(n_det_remaining_1h_or_1p) = iorb
   do k = 1, N_int
    psi_det_remaining_1h_or_1p(k,1,n_det_remaining_1h_or_1p) = key_tmp(k,1)
    psi_det_remaining_1h_or_1p(k,2,n_det_remaining_1h_or_1p) = key_tmp(k,2)
   enddo
!  do k = 1, n_det_total
!    call i_h_j(psi_det_final(1,1,k),key_tmp,N_int,hij)
!    H_array(k) = hij
!  enddo
!  do k = 1, n_det_total 
!   do l = 1, n_det_total
!    H_matrix_total(k,l) += H_array(k) * H_array(l) * href
!   enddo
!  enddo
  enddo
 enddo
 print*,'n_det_remaining_1h_or_1p = ',n_det_remaining_1h_or_1p
 deallocate(key_tmp,H_array)

 double precision, allocatable :: eigvalues_bis(:),eigvectors_bis(:,:),H_matrix_total_bis(:,:)
 integer :: n_det_final
 n_det_final = n_det_total + n_det_remaining_1h_or_1p
 allocate(eigvalues_bis(n_det_final),eigvectors_bis(n_det_final,n_det_final),H_matrix_total_bis(n_det_final,n_det_final))
 print*,'passed the allocate, building the big matrix'
 do i = 1, n_det_total 
  do j = 1, n_det_total
   H_matrix_total_bis(i,j) = H_matrix_total(i,j)
  enddo
 enddo
 do i = 1, n_det_remaining_1h_or_1p
  do j = 1, n_det_remaining_1h_or_1p
   call i_h_j(psi_det_remaining_1h_or_1p(1,1,i),psi_det_remaining_1h_or_1p(1,1,j),N_int,hij)
   H_matrix_total_bis(n_det_total+i,n_det_total+j) = hij
  enddo
 enddo
 do i = 1, n_det_total
  do j = 1, n_det_remaining_1h_or_1p
   call i_h_j(psi_det_final(1,1,i),psi_det_remaining_1h_or_1p(1,1,j),N_int,hij)
   H_matrix_total_bis(i,n_det_total+j) = hij 
   H_matrix_total_bis(n_det_total+j,i) = hij 
  enddo
 enddo
 print*,'passed the matrix'
 do i = 1, n_det_remaining_1h_or_1p
   if(one_hole_or_one_p_bis(i))then
    H_matrix_total_bis(n_det_total+i,n_det_total+i) += total_corr_e_2h2p -corr_energy_2h2p_per_orb_ab(holes_or_particle_bis(i)) &
                                                                         -corr_energy_2h2p_per_orb_bb(holes_or_particle_bis(i))  
    H_matrix_total_bis(n_det_total+i,n_det_total+i) += total_corr_e_1h2p -corr_energy_1h2p_per_orb_ab(holes_or_particle_bis(i)) &  
                                                                         -corr_energy_1h2p_per_orb_bb(holes_or_particle_bis(i))  
   else
    H_matrix_total_bis(n_det_total+i,n_det_total+i) += total_corr_e_2h2p -corr_energy_2h2p_per_orb_ab(holes_or_particle_bis(i)) &
                                                                         -corr_energy_2h2p_per_orb_aa(holes_or_particle_bis(i))  
    H_matrix_total_bis(n_det_total+i,n_det_total+i) += total_corr_e_1h2p -corr_energy_2h1p_per_orb_ab(holes_or_particle_bis(i)) &  
                                                                         -corr_energy_2h1p_per_orb_aa(holes_or_particle_bis(i))  
 
   endif
 enddo
 do i = 2, n_det_final
  do  j = i+1, n_det_final
   H_matrix_total_bis(i,j) = 0.d0
   H_matrix_total_bis(j,i) = 0.d0
  enddo
 enddo
 do i = 1, n_det_final
   write(*,'(500(F10.5,X))')H_matrix_total_bis(i,:)
 enddo
 call lapack_diag(eigvalues_bis,eigvectors_bis,H_matrix_total_bis,n_det_final,n_det_final)
 print*,'e_dressed  = ',eigvalues_bis(1) + nuclear_repulsion
 do i = 1, n_det_final
  print*,'coef = ',eigvectors_bis(i,1),H_matrix_total_bis(i,i) - H_matrix_total_bis(1,1)
 enddo
 do j = 1, N_states
  do i = 1, n_det_total
   psi_coef_final(i,j) = eigvectors_bis(i,j)
   norm += psi_coef_final(i,j)**2
  enddo
  norm = 1.d0/dsqrt(norm)
  do i = 1, n_det_total
   psi_coef_final(i,j) = psi_coef_final(i,j) * norm
  enddo
 enddo


 deallocate(eigvalues_bis,eigvectors_bis,H_matrix_total_bis)


!print*,'H matrix to diagonalize'
!href = H_matrix_total(1,1)
!do i = 1, n_det_total
! H_matrix_total(i,i) -= href
!enddo
!do i = 1, n_det_total
!  write(*,'(100(X,F16.8))')H_matrix_total(i,:)
!enddo
!call lapack_diag(eigvalues,eigvectors,H_matrix_total,n_det_total,n_det_total)
!print*,'H_matrix_total(1,1) = ',H_matrix_total(1,1)
!print*,'e_dressed  = ',eigvalues(1) + nuclear_repulsion
!do i = 1, n_det_total
! print*,'coef = ',eigvectors(i,1),H_matrix_total(i,i) - H_matrix_total(1,1)
!enddo
!norm = 0.d0
!do i = 1, n_det_total
! do j = 1, N_states
!  psi_coef_final(i,j) = eigvectors(i,j)
! enddo
! norm += psi_coef_final(i,1)**2
!enddo
!print*,'norm = ',norm
  
 call set_psi_det_as_input_psi(n_det_total,psi_det_final,psi_coef_final)

 do i = 1, N_det
   call debug_det(psi_det(1,1,i),N_int)
   print*,'coef = ',psi_coef(i,1)
 enddo
 provide one_body_dm_mo

 integer :: i_core,iorb,jorb,i_inact,j_inact,i_virt,j_virt,j_core
 do i = 1, n_core_orb
  i_core = list_core(i)
  one_body_dm_mo(i_core,i_core) = 10.d0
  do j = i+1, n_core_orb
   j_core = list_core(j)
   one_body_dm_mo(i_core,j_core) = 0.d0
   one_body_dm_mo(j_core,i_core) = 0.d0
  enddo
  do j = 1, n_inact_orb
   iorb = list_inact(j)
   one_body_dm_mo(i_core,iorb) = 0.d0
   one_body_dm_mo(iorb,i_core) = 0.d0
  enddo
  do j = 1, n_act_orb
   iorb = list_act(j)
   one_body_dm_mo(i_core,iorb) = 0.d0
   one_body_dm_mo(iorb,i_core) = 0.d0
  enddo
  do j = 1, n_virt_orb
   iorb = list_virt(j)
   one_body_dm_mo(i_core,iorb) = 0.d0
   one_body_dm_mo(iorb,i_core) = 0.d0
  enddo
 enddo
 ! Set to Zero the inact-inact part to avoid arbitrary rotations
 do i = 1, n_inact_orb
  i_inact = list_inact(i)
  do j = i+1, n_inact_orb 
   j_inact = list_inact(j)
   one_body_dm_mo(i_inact,j_inact) = 0.d0
   one_body_dm_mo(j_inact,i_inact) = 0.d0
  enddo
 enddo

 ! Set to Zero the inact-virt part to avoid arbitrary rotations
 do i = 1, n_inact_orb
  i_inact = list_inact(i)
  do j = 1, n_virt_orb 
   j_virt = list_virt(j)
   one_body_dm_mo(i_inact,j_virt) = 0.d0
   one_body_dm_mo(j_virt,i_inact) = 0.d0
  enddo
 enddo

 ! Set to Zero the virt-virt part to avoid arbitrary rotations
 do i = 1, n_virt_orb
  i_virt = list_virt(i)
  do j = i+1, n_virt_orb 
   j_virt = list_virt(j)
   one_body_dm_mo(i_virt,j_virt) = 0.d0
   one_body_dm_mo(j_virt,i_virt) = 0.d0
  enddo
 enddo


 print*,''
 print*,'Inactive-active Part of the One body DM'
 print*,''
 do i = 1,n_act_orb
  iorb = list_act(i)
  print*,''
  print*,'ACTIVE ORBITAL  ',iorb
  do j = 1, n_inact_orb
   jorb = list_inact(j)
   if(dabs(one_body_dm_mo(iorb,jorb)).gt.threshold_lmct)then
    print*,'INACTIVE  '
    print*,'DM ',iorb,jorb,dabs(one_body_dm_mo(iorb,jorb))
   endif
  enddo
  do j = 1, n_virt_orb
   jorb = list_virt(j)
   if(dabs(one_body_dm_mo(iorb,jorb)).gt.threshold_mlct)then
    print*,'VIRT      '
    print*,'DM ',iorb,jorb,dabs(one_body_dm_mo(iorb,jorb))
   endif
  enddo
 enddo
 do i = 1, mo_tot_num
  do j = i+1, mo_tot_num
   if(dabs(one_body_dm_mo(i,j)).le.threshold_fobo_dm)then
      one_body_dm_mo(i,j) = 0.d0
      one_body_dm_mo(j,i) = 0.d0
   endif
  enddo
 enddo






 label = "Natural"
 character*(64) :: label
 integer :: sign
 sign = -1
 
 call mo_as_eigvectors_of_mo_matrix(one_body_dm_mo,size(one_body_dm_mo,1),size(one_body_dm_mo,2),label,sign,.true.)
 soft_touch mo_coef
 call save_mos
 
 deallocate(eigvalues,eigvectors,psi_det_final,psi_coef_final)




 deallocate(H_matrix_total)
 deallocate(psi_good_det)
 deallocate(dressing_restart_good_det)
 deallocate(dressing_matrix_restart_1h1p)
 deallocate(dressing_matrix_restart_2h1p)
 deallocate(dressing_diag_good_det)

end


