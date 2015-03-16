use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("just_mono")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_double_excitations()
print s

s = H_apply("h_core_just_mono")
s.set_selection_pt2("h_core")
s.unset_double_excitations()
print s


s = H_apply("just_mono_no_double_hole_pt2")
s.set_perturbation("epstein_nesbet_2x2")
s.unset_double_excitations()
s.set_filter_holes()
print s


END_SHELL

