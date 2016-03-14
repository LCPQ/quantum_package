program restart_more_singles
  BEGIN_DOC
  ! Generates and select single excitations 
  ! on the top of a given restart wave function
  END_DOC
  read_wf = .true.
  touch read_wf 
  call routine

end 
subroutine routine
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision,allocatable :: E_before(:)
  integer :: n_det_before
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
  i = 0
  print*,'N_det = ',N_det
  print*,'n_det_max = ',n_det_max
  print*,'pt2_max = ',pt2_max
  pt2=-1.d0
  E_before = ref_bitmask_energy
  pt2_max = 1.d-10
  n_det_max = 200000
  do while (N_det < n_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    n_det_before = N_det
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_just_mono(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy
    print*,'pt2      = ',pt2
    print*,'E+PT2    = ',E_before + pt2
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
    call save_wavefunction
    
    if(n_det_before == N_det)then
     selection_criterion = selection_criterion * 0.5d0
    endif
  enddo
  
  threshold_davidson = 1.d-10
  soft_touch threshold_davidson davidson_criterion
  call diagonalize_CI
  if(N_states_diag.gt.1)then
   print*,'Variational Energy difference'
   do i = 2, N_st
    print*,'Delta E = ',CI_energy(i) - CI_energy(1)
   enddo
  endif
  if(N_states.gt.1)then
   print*,'Variational + perturbative Energy difference'
   do i = 2, N_st
    print*,'Delta E = ',CI_energy(i)+ pt2(i) - (CI_energy(1) + pt2(1))
   enddo
  endif
  call save_wavefunction
  deallocate(pt2,norm_pert,E_before)
end
