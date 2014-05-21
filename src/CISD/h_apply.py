#!/usr/bin/env python

from generate_h_apply import *

s = H_apply("cisd",openmp=True)
s["keys_work"]  += """
call fill_H_apply_buffer_cisd(key_idx,keys_out,N_int)
""" 
print s


