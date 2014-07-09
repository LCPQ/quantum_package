use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("FCI")
s.set_selection_pt2("epstein_nesbet")
print s

s = H_apply("FCI_PT2")
s.set_perturbation("epstein_nesbet")
print s

END_SHELL

