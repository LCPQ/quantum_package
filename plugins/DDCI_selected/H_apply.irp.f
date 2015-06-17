use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("DDCI_selection")
s.set_selection_pt2("epstein_nesbet_2x2")
s.set_filter_2h_2p()
print s

s = H_apply("DDCI_PT2")
s.set_perturbation("epstein_nesbet_2x2")
s.set_filter_2h_2p()
print s

END_SHELL

