use bitmasks
BEGIN_SHELL [ /usr/bin/env python2 ]
from generate_h_apply import *
from perturbation import perturbations

s = H_apply("mp2")
s.set_perturbation("Moller_plesset")
print s

s = H_apply("mp2_selection")
s.set_selection_pt2("Moller_Plesset")
print s
END_SHELL

