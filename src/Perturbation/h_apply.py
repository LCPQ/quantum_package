#!/usr/bin/env python

from generate_h_apply import *

s = H_apply("cisd_pt2",openmp=True)
s.set_perturbation("epstein_nesbet_2x2")
#s.set_perturbation("epstein_nesbet")
#s["keys_work"]  += """
#call fill_H_apply_buffer_cisd(key_idx,keys_out,N_int)
#""" 
print s


s = H_apply("cisd_selection",openmp=True)
s.set_selection_pt2("epstein_nesbet_2x2")
print s
