use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("FCI",openmp=True)
s.set_selection_pt2("epstein_nesbet_2x2")
print s

END_SHELL

