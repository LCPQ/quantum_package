program restart_more_singles
  ! Generates and select single excitations 
  ! on the top of a given restart wave function
  read_wf = .true.
  touch read_wf 
  call routine

end 
subroutine routine
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  double precision :: E_before
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  selection_criterion = 0.d0
  soft_touch selection_criterion
  i = 0
  print*,'N_det = ',N_det
  print*,'n_det_max_fci = ',n_det_max_fci
  print*,'pt2_max = ',pt2_max
  pt2=-1.d0
  E_before = ref_bitmask_energy
  do while (N_det < n_det_max_fci.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    i += 1
    print*,'-----------------------'
    print*,'i = ',i
    call H_apply_just_mono(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    print*,'E        = ',CI_energy(1)
    print*,'pt2      = ',pt2(1)
    print*,'E+PT2    = ',E_before + pt2(1)
    E_before = CI_energy(1)
    
  enddo
  call save_wavefunction
  deallocate(pt2,norm_pert)
end
