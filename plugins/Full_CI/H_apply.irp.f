use bitmasks
BEGIN_SHELL [ /usr/bin/env python2 ]
from generate_h_apply import *

s = H_apply("FCI")
s.set_selection_pt2("epstein_nesbet_2x2")
#s.set_selection_pt2("qdpt")
print s

s = H_apply("FCI_PT2")
s.set_perturbation("epstein_nesbet_2x2")
#s.set_perturbation("qdpt")
s.unset_openmp()
print s

s = H_apply("FCI_no_selection")
s.set_selection_pt2("dummy")
print s

s = H_apply("FCI_mono")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_double_excitations()
s.unset_openmp()
print s



END_SHELL

