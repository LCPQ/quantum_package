use bitmasks
BEGIN_SHELL [ /usr/bin/env python2 ]
from generate_h_apply import *
from perturbation import perturbations

s = H_apply("SC2_selected",SingleRef=True)
s.set_selection_pt2("epstein_nesbet_sc2_no_projected")
print s

s = H_apply("PT2",SingleRef=True)
s.set_perturbation("epstein_nesbet_sc2_no_projected")
print s

s = H_apply("PT2_en_sc2",SingleRef=True)
s.set_perturbation("epstein_nesbet_sc2")
print s
END_SHELL

