subroutine all_single(e_pt2)
  implicit none
  double precision, intent(in)   :: e_pt2
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision,allocatable :: E_before(:)
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
  if(.not.selected_fobo_ci)then
   selection_criterion = 0.d0
   soft_touch selection_criterion
  else 
   selection_criterion =  0.1d0 
   selection_criterion_factor = 0.01d0
   selection_criterion_min = selection_criterion
   soft_touch selection_criterion
  endif
  print*, 'e_pt2 = ',e_pt2
  pt2_max = 0.15d0 * e_pt2
  soft_touch pt2_max
  print*, 'pt2_max = ',pt2_max
  threshold_davidson = 1.d-9
  soft_touch threshold_davidson davidson_criterion
  i = 0
  print*,'Doing all the mono excitations !'
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  print*,'N_det_generators = ',N_det_generators
  pt2=-1.d0
  print*, 'ref_bitmask_energy   =',ref_bitmask_energy
  print*, 'CI_expectation_value =',psi_energy(1)
  E_before = ref_bitmask_energy
 
  print*,'Initial Step '
  print*,'Inital determinants '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  n_det_max = 100000
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > dabs(pt2_max))
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_just_mono(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    print*,'pt2_max  = ',pt2_max
    print*, maxval(abs(pt2(1:N_st))) > dabs(pt2_max)
    if(N_states_diag.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',E_before(i)+ pt2(i) - (E_before(1) + pt2(1))
     enddo
    endif
    E_before = CI_energy
    !!!!!!!!!!!!!!!!!!!!!!!!!!! DOING ONLY ONE ITERATION OF SELECTION AS THE SELECTION CRITERION IS SET TO ZERO
  enddo
! threshold_davidson = 1.d-8
! soft_touch threshold_davidson davidson_criterion
! call diagonalize_CI
  print*,'Final Step '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  do i = 1, max(2,N_det_generators)
   print*,'psi_coef = ',psi_coef(i,1)
  enddo
  deallocate(pt2,norm_pert,E_before)
end

subroutine all_1h2p
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision,allocatable :: E_before(:)
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
  selection_criterion = 0.d0
  soft_touch selection_criterion
  threshold_davidson = 1.d-5
  soft_touch threshold_davidson davidson_criterion
  i = 0
  print*,''
  print*,''
  print*,''
  print*,''
  print*,''
  print*,'*****************************'
  print*,'Doing all the 1h2P excitations'
  print*,'*****************************'
  print*,''
  print*,''
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  print*,'N_det_generators = ',N_det_generators
  pt2=-1.d0
  E_before = ref_bitmask_energy
 
  print*,'Initial Step '
  print*,'Inital determinants '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  n_det_max = 100000
  i = 0
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_only_1h2p(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    if(N_states_diag.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',E_before(i)+ pt2(i) - (E_before(1) + pt2(1))
     enddo
    endif
    E_before = CI_energy
    
  enddo
  print*,'Final Step '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo

  do i = 1, 2
   print*,'psi_coef = ',psi_coef(i,1)
  enddo
  deallocate(pt2,norm_pert,E_before)
end

subroutine all_2h2p
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision,allocatable :: E_before(:)
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
  selection_criterion = 0.d0
  soft_touch selection_criterion
  threshold_davidson = 1.d-5
  soft_touch threshold_davidson davidson_criterion
  i = 0
  print*,''
  print*,''
  print*,''
  print*,''
  print*,''
  print*,'*****************************'
  print*,'Doing all the 2h2P excitations'
  print*,'*****************************'
  print*,''
  print*,''
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  print*,'N_det_generators = ',N_det_generators
  pt2=-1.d0
  E_before = ref_bitmask_energy
 
  print*,'Initial Step '
  print*,'Inital determinants '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  n_det_max = 100000
  i = 0
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_only_2h2p(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    if(N_states_diag.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',E_before(i)+ pt2(i) - (E_before(1) + pt2(1))
     enddo
    endif
    E_before = CI_energy
    
  enddo
  print*,'Final Step '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  do i = 1, 2
   print*,'psi_coef = ',psi_coef(i,1)
  enddo
  deallocate(pt2,norm_pert,E_before)
end

subroutine all_2p
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision,allocatable :: E_before(:)
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
  selection_criterion = 0.d0
  soft_touch selection_criterion
  threshold_davidson = 1.d-5
  soft_touch threshold_davidson davidson_criterion
  i = 0
  print*,''
  print*,''
  print*,''
  print*,''
  print*,''
  print*,'*****************************'
  print*,'Doing all the 2P excitations'
  print*,'*****************************'
  print*,''
  print*,''
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  print*,'N_det_generators = ',N_det_generators
  pt2=-1.d0
  E_before = ref_bitmask_energy
 
  print*,'Initial Step '
  print*,'Inital determinants '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  n_det_max = 100000
  i = 0
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_only_2p(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    if(N_states_diag.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',E_before(i)+ pt2(i) - (E_before(1) + pt2(1))
     enddo
    endif
    E_before = CI_energy
    
  enddo
  print*,'Final Step '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  deallocate(pt2,norm_pert,E_before)
  do i = 1, 2
   print*,'psi_coef = ',psi_coef(i,1)
  enddo
end

subroutine all_single_no_1h_or_1p
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision,allocatable :: E_before(:)
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
  selection_criterion = 0.d0
  soft_touch selection_criterion
  threshold_davidson = 1.d-5
  soft_touch threshold_davidson davidson_criterion
  i = 0
  print*,'Doing all the mono excitations !'
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  print*,'N_det_generators = ',N_det_generators
  pt2=-1.d0
  E_before = ref_bitmask_energy
 
  print*,'Initial Step '
  print*,'Inital determinants '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  n_det_max = 100000
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_just_mono_no_1h_no_1p(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    if(N_states_diag.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',E_before(i)+ pt2(i) - (E_before(1) + pt2(1))
     enddo
    endif
    E_before = CI_energy
  enddo
  threshold_davidson = 1.d-16
  soft_touch threshold_davidson davidson_criterion
  call diagonalize_CI
  print*,'Final Step '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  do i = 1, 2
   print*,'psi_coef = ',psi_coef(i,1)
  enddo
! call save_wavefunction
  deallocate(pt2,norm_pert,E_before)
end

subroutine all_single_no_1h_or_1p_or_2p
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision,allocatable :: E_before(:)
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
  selection_criterion = 0.d0
  soft_touch selection_criterion
  threshold_davidson = 1.d-5
  soft_touch threshold_davidson davidson_criterion
  i = 0
  print*,'Doing all the mono excitations !'
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  print*,'N_det_generators = ',N_det_generators
  pt2=-1.d0
  E_before = ref_bitmask_energy
 
  print*,'Initial Step '
  print*,'Inital determinants '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  n_det_max = 100000
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_just_mono_no_1h_no_1p_no_2p(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    if(N_states_diag.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',E_before(i)+ pt2(i) - (E_before(1) + pt2(1))
     enddo
    endif
    E_before = CI_energy
  enddo
  threshold_davidson = 1.d-10
  soft_touch threshold_davidson davidson_criterion
  call diagonalize_CI
  print*,'Final Step '
  print*,'N_det = ',N_det
  do i = 1, N_states_diag
   print*,''
   print*,'i = ',i
   print*,'E        = ',CI_energy(i)
   print*,'S^2      = ',CI_eigenvectors_s2(i)
  enddo
  do i = 1, 2
   print*,'psi_coef = ',psi_coef(i,1)
  enddo
! call save_wavefunction
  deallocate(pt2,norm_pert,E_before)
end

subroutine all_1h_1p_routine
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision :: E_before
  integer :: n_det_before
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  i = 0
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  pt2=-1.d0
  E_before = ref_bitmask_energy
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    n_det_before = N_det
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_just_1h_1p(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    E_before = CI_energy(1)
    if(n_det_before == N_det)then
     selection_criterion = selection_criterion * 0.5d0
    endif
  enddo
  deallocate(pt2,norm_pert)
end
subroutine all_but_1h_1p_routine
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision :: E_before
  integer :: n_det_before
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  i = 0
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  pt2=-1.d0
  E_before = ref_bitmask_energy
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    n_det_before = N_det
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_all_but_1h_and_1p(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    E_before = CI_energy(1)
    if(n_det_before == N_det)then
     selection_criterion = selection_criterion * 0.5d0
    endif
  enddo
  deallocate(pt2,norm_pert)
end
