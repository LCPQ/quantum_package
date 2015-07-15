use bitmasks
BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import *

s = H_apply("mrcc")
s.data["parameters"] = ", delta_ij_, delta_ii_,Ndet_cas, Ndet_non_cas"
s.data["declarations"] += """
    integer, intent(in) :: Ndet_cas,Ndet_non_cas
    double precision, intent(in) :: delta_ij_(Ndet_cas,Ndet_non_cas,*)
    double precision, intent(in) :: delta_ii_(Ndet_cas,*)
"""
s.data["keys_work"] = "call mrcc_dress(delta_ij_,delta_ii_,Ndet_cas,Ndet_non_cas,i_generator,key_idx,keys_out,N_int,iproc)"
s.data["params_post"] += ", delta_ij_, delta_ii_, Ndet_cas, Ndet_non_cas"
s.data["params_main"] += "delta_ij_, delta_ii_, Ndet_cas, Ndet_non_cas"
s.data["decls_main"] += """
    integer, intent(in) :: Ndet_cas,Ndet_non_cas
    double precision, intent(in) :: delta_ij_(Ndet_cas,Ndet_non_cas,*)
    double precision, intent(in) :: delta_ii_(Ndet_cas,*)
"""
s.data["finalization"] = ""
s.data["copy_buffer"] = ""
s.data["generate_psi_guess"] = ""
s.data["size_max"] = "3072"
s.filter_vvvv_excitation()
print s


END_SHELL

