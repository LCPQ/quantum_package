use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("mrpt")
s.data["parameters"] = ", delta_ij_, Ndet"
s.data["declarations"] += """
    integer, intent(in) :: Ndet
    double precision, intent(in) :: delta_ij_(Ndet,Ndet,*)
"""
s.data["keys_work"] = "call mrpt_dress(delta_ij_,Ndet,i_generator,key_idx,keys_out,N_int,iproc,key_mask)"
s.data["params_post"] += ", delta_ij_, Ndet"
s.data["params_main"] += "delta_ij_,  Ndet"
s.data["decls_main"] += """
    integer, intent(in) :: Ndet
    double precision, intent(in) :: delta_ij_(Ndet,Ndet,*)
"""
s.data["finalization"] = ""
s.data["copy_buffer"] = ""
s.data["generate_psi_guess"] = ""
s.data["size_max"] = "3072"
print s


END_SHELL

