use bitmasks
BEGIN_SHELL [ /usr/bin/env python2 ]
from generate_h_apply import *

s = H_apply("mrcc")
s.data["parameters"] = ", delta_ij_, Nstates, Ndet_non_ref, Ndet_ref"
s.data["declarations"] += """
    integer, intent(in) :: Nstates, Ndet_ref, Ndet_non_ref
    double precision, intent(in) :: delta_ij_(Nstates, Ndet_non_ref, Ndet_ref)
"""
s.data["keys_work"] = "call mrcc_dress(delta_ij_,Nstates,Ndet_non_ref,Ndet_ref,i_generator,key_idx,keys_out,N_int,iproc,key_mask)"
s.data["params_post"] += ", delta_ij_, Nstates, Ndet_non_ref, Ndet_ref"
s.data["params_main"] += "delta_ij_, Nstates, Ndet_non_ref, Ndet_ref"
s.data["decls_main"] += """
    integer, intent(in) :: Ndet_ref, Ndet_non_ref, Nstates
    double precision, intent(in) :: delta_ij_(Nstates,Ndet_non_ref,Ndet_ref)
"""
s.data["finalization"] = ""
s.data["copy_buffer"] = ""
s.data["generate_psi_guess"] = ""
s.data["size_max"] = "3072"
print s



s = H_apply("mrcc_PT2")
s.energy = "ci_electronic_energy_dressed"
s.set_perturbation("epstein_nesbet_2x2")
s.unset_openmp()
print s

s = H_apply("mrcepa_PT2")
s.energy = "psi_energy"
s.set_perturbation("epstein_nesbet_2x2")
s.unset_openmp()
print s

END_SHELL

