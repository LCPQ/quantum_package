use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("mrcc_simple")
s.data["parameters"] = ", delta_ij_sd_, Ndet_sd"
s.data["declarations"] += """
    integer, intent(in) :: Ndet_sd
    double precision, intent(in) :: delta_ij_sd_(Ndet_sd,Ndet_sd,*)
"""
s.data["keys_work"] = "call mrcc_dress_simple(delta_ij_sd_,Ndet_sd,i_generator,key_idx,keys_out,N_int,iproc)"
s.data["params_post"] += ", delta_ij_sd_, Ndet_sd"
s.data["params_main"] += "delta_ij_sd_, Ndet_sd"
s.data["decls_main"] += """
    integer, intent(in) :: Ndet_sd
    double precision, intent(in) :: delta_ij_sd_(Ndet_sd,Ndet_sd,*)
"""
s.data["finalization"] = ""
s.data["copy_buffer"] = ""
s.data["generate_psi_guess"] = ""
s.data["size_max"] = "3072"
print s




s.data["subroutine"] = "H_apply_mrcc"
s.data["keys_work"] = "call mrcc_dress(delta_ij_sd_,Ndet_sd,i_generator,key_idx,keys_out,N_int,iproc)"
print s

END_SHELL

