program full_ci
  implicit none
  integer                        :: i,k

  
  print *,  '===================================================================='
  print *,  'This program is slow. Consider using the Full_CI_ZMQ module instead.'
  print *,  '===================================================================='
  call sleep(2)

  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  character*(64)                 :: perturbation
  
  pt2 = 1.d0
  diag_algorithm = "Lapack"

  if (N_det > N_det_max) then
    call diagonalize_CI
    call save_wavefunction
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = N_det_max
    soft_touch N_det psi_det psi_coef
    call diagonalize_CI
    call save_wavefunction
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
    print *,  '-----'
  endif
  double precision :: i_H_psi_array(N_states),diag_H_mat_elem,h,i_O1_psi_array(N_states)
  double precision :: E_CI_before(N_states)
  if(read_wf)then
   call i_H_psi(psi_det(1,1,N_det),psi_det,psi_coef,N_int,N_det,psi_det_size,N_states,i_H_psi_array)
   h = diag_H_mat_elem(psi_det(1,1,N_det),N_int)
   selection_criterion = dabs(psi_coef(N_det,1) *  (i_H_psi_array(1) - h * psi_coef(N_det,1))) * 0.1d0
   soft_touch selection_criterion
  endif


  integer :: n_det_before
  print*,'Beginning the selection ...'
  E_CI_before(1:N_states) = CI_energy(1:N_states)
  do while (N_det < N_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    n_det_before = N_det
    call H_apply_FCI(pt2, norm_pert, H_pert_diag,  N_st)

    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    call diagonalize_CI

    if (N_det > N_det_max) then
      N_det = N_det_max
      psi_det = psi_det_sorted
      psi_coef = psi_coef_sorted
      touch N_det psi_det psi_coef psi_det_sorted psi_coef_sorted psi_average_norm_contrib_sorted
    endif


    call save_wavefunction
    if(n_det_before == N_det)then
     selection_criterion = selection_criterion * 0.5d0
    endif
    print *,  'N_det          = ', N_det
    print *,  'N_states       = ', N_states
    do  k = 1, N_states
    print*,'State ',k
    print *,  'PT2            = ', pt2(k)
    print *,  'E              = ', CI_energy(k)
    print *,  'E(before)+PT2  = ', E_CI_before(k)+pt2(k)
    enddo
    print *,  '-----'
    if(N_states.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_states
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_states
      print*,'Delta E = ',E_CI_before(i)+ pt2(i) - (E_CI_before(1) + pt2(1))
     enddo
    endif
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    call ezfio_set_full_ci_energy(CI_energy(1))
  enddo
   N_det = min(N_det_max,N_det)
   touch N_det psi_det psi_coef
   call diagonalize_CI
   if(do_pt2)then
    print*,'Last iteration only to compute the PT2'
    threshold_generators = threshold_generators_pt2
    threshold_selectors = threshold_selectors_pt2
    SOFT_TOUCH threshold_generators threshold_selectors
    call H_apply_FCI_PT2(pt2, norm_pert, H_pert_diag,  N_st)
 
    print *,  'Final step'
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy(1:N_states)
    print *,  'E+PT2    = ', CI_energy(1:N_states)+pt2(1:N_states)
    print *,  '-----'
    call ezfio_set_full_ci_energy_pt2(CI_energy(1)+pt2(1))
   endif
   call save_wavefunction
  deallocate(pt2,norm_pert)
end
