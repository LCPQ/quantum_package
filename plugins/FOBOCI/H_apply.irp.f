use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("just_1h_1p")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_only_1h1p()
print s


s = H_apply("all_but_1h_and_1p")
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_1h()
s.filter_1p()
print s



s = H_apply("only_1h2p")
s.set_selection_pt2("epstein_nesbet")
s.filter_only_1h2p()
s.unset_skip()
print s

s = H_apply("only_2h2p")
s.set_selection_pt2("epstein_nesbet")
s.filter_only_2h2p()
s.unset_skip()
print s


s = H_apply("only_2p")
s.set_selection_pt2("epstein_nesbet")
s.filter_only_2p()
s.unset_skip()
print s

s = H_apply("just_mono",do_double_exc=False)
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
print s



s = H_apply("just_mono_no_1h_no_1p",do_double_exc=False)
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_1h()
s.filter_1p()
print s

s = H_apply("just_mono_no_1h_no_1p_no_2p",do_double_exc=False)
s.set_selection_pt2("epstein_nesbet_2x2")
s.unset_skip()
s.filter_1h()
s.filter_1p()
s.filter_2p()
print s


END_SHELL

