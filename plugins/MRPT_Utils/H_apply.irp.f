use bitmasks
BEGIN_SHELL [ /usr/bin/env python2 ]
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

s = H_apply("mrpt_1h")
s.filter_only_1h()
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

s = H_apply("mrpt_1p")
s.filter_only_1p()
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

s = H_apply("mrpt_1h1p")
s.filter_only_1h1p()
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

s = H_apply("mrpt_2p")
s.filter_only_2p()
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

s = H_apply("mrpt_2h")
s.filter_only_2h()
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


s = H_apply("mrpt_1h2p")
s.filter_only_1h2p()
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

s = H_apply("mrpt_2h1p")
s.filter_only_2h1p()
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

s = H_apply("mrpt_2h2p")
s.filter_only_2h2p()
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

