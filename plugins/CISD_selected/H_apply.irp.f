use bitmasks
BEGIN_SHELL [ /usr/bin/env python2 ]
from generate_h_apply import *
from perturbation import perturbations

for perturbation in perturbations:
  s = H_apply("cisd_selection_"+perturbation)
  s.set_selection_pt2(perturbation)
  print s
END_SHELL


subroutine H_apply_cisd_selection(perturbation,pt2, norm_pert, H_pert_diag, N_st)
  implicit none
  character*(64), intent(in) :: perturbation
  integer, intent(in)            :: N_st
  double precision, intent(inout):: pt2(N_st), norm_pert(N_st), H_pert_diag(N_st)

BEGIN_SHELL [  /usr/bin/env python2 ]
from perturbation import perturbations

for perturbation in perturbations:
  print """
  if (perturbation == '%s') then
    call H_apply_cisd_selection_%s(pt2, norm_pert, H_pert_diag,  N_st)
  endif
  """%(perturbation,perturbation)
END_SHELL


end
