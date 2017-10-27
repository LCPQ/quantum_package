use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("just_1h_1p")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_only_1h1p()
print s


s = H_apply("just_2p")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_only_2p()
print s


s = H_apply("just_1p")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_only_1p()
print s



s = H_apply("just_1h_1p_singles",do_double_exc=False)
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_only_1h1p()
print s


s = H_apply("just_mono",do_double_exc=False)
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
print s

END_SHELL

