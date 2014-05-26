program cisd
  implicit none
  integer                        :: i,k
  double precision, allocatable  :: eigvalues(:),eigvectors(:,:)

  
  double precision, allocatable  :: pt2(:), norm_pert(:)
  double precision               :: H_pert_diag, E_old
  integer                        :: N_st, iter
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st))
  allocate(eigvalues(n_states),eigvectors(n_det,n_states))
  
  pt2 = 1.d0
  do while (maxval(abs(pt2(1:N_st))) > 1.d-6)
    E_old = CI_energy(1)
    call H_apply_cisd_selection(pt2, norm_pert, H_pert_diag,  N_st)
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', E_old
    print *,  'E+PT2    = ', E_old+pt2
  enddo
end
