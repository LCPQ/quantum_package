use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("FCI")
s.set_selection_pt2("epstein_nesbet_2x2")
print s

s = H_apply("FCI_PT2")
s.set_perturbation("epstein_nesbet_2x2")
print s


s = H_apply("FCI_mono")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_double_excitations()
print s


s = H_apply("select_mono_delta_rho")
s.unset_double_excitations()
s.set_selection_pt2("delta_rho_one_point")
print s

s = H_apply("pt2_mono_delta_rho")
s.unset_double_excitations()
s.set_perturbation("delta_rho_one_point")
print s

s = H_apply("select_mono_di_delta_rho")
s.set_selection_pt2("delta_rho_one_point")
print s

s = H_apply("pt2_mono_di_delta_rho")
s.set_perturbation("delta_rho_one_point")
print s


END_SHELL

