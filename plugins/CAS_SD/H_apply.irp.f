use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("CAS_SD")
print s

s = H_apply("CAS_SD_selected_no_skip")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
print s

s = H_apply("CAS_SD_selected")
s.set_selection_pt2("epstein_nesbet_2x2")
print s

s = H_apply("CAS_SD_PT2")
s.set_perturbation("epstein_nesbet_2x2")
print s


s = H_apply("CAS_S",do_double_exc=False)
print s

s = H_apply("CAS_S_selected_no_skip",do_double_exc=False)
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
print s

s = H_apply("CAS_S_selected",do_double_exc=False)
s.set_selection_pt2("epstein_nesbet_2x2")
print s

s = H_apply("CAS_S_PT2",do_double_exc=False)
s.set_perturbation("epstein_nesbet_2x2")
print s

END_SHELL

