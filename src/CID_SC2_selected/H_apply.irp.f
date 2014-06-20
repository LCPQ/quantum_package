use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *
from perturbation import perturbations

s = H_apply("PT2",SingleRef=True,do_mono_exc=False,do_double_exc=True)
s.set_perturbation("epstein_nesbet_sc2_projected")
print s
END_SHELL

