#!/usr/bin/env python

import generate_h_apply

# H_apply
s = generate_h_apply.new_dict(openmp=True)
s["subroutine"]       = "H_apply_cisd"
s["keys_work"]        = "call fill_H_apply_buffer_cisd(key_idx,keys_out,N_int)"
generate_h_apply.create_h_apply(s) 


