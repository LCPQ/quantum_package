program cisd
  implicit none
  integer :: i,k
  double precision, allocatable  :: eigvalues(:),eigvectors(:,:)
  double precision, allocatable :: pt2(:), norm_pert(:)
  double precision :: H_pert_diag
  integer :: N_st
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st))

  call H_apply_cisd_pt2(pt2, norm_pert, H_pert_diag,  N_st)
  print *,  'N_det    = ', N_det
  print *,  'N_states = ', N_states
  print *,  'pt2      = ', pt2
  print *,  'E        = ', reference_energy+pt2+nuclear_repulsion
  return
end
