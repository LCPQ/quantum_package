use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("mrcc")
s.data["keys_work"] = "call mrcc_dress(i_generator,key_idx,keys_out,N_int,iproc)"
print s

END_SHELL

