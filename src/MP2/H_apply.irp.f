use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *
from perturbation import perturbations

s = H_apply("mp2",openmp=True)
s.set_perturbation("Moller_plesset")
print s
END_SHELL

