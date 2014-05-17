#!/usr/bin/env python

import os
file = open(os.environ["QPACKAGE_ROOT"]+'/src/Dets/H_apply_template.f','r')
template = file.read()
file.close()

keywords = """
subroutine
parameters
initialization
declarations
keys_work
finalization
""".split()

class H_apply(object):

  def __init__(self,sub,openmp=True):
    s = {}
    for k in keywords:
      s[k] = ""
    s["subroutine"] = "H_apply_%s"%(sub)
    self.openmp = openmp
    if openmp:
      s["subroutine"] += "_OpenMP"
    self.perturbation = None
   #s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(NONE)          &
    s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(SHARED)        &
        !$OMP PRIVATE(i,j,k,l,keys_out,hole,particle,                &
        !$OMP  occ_particle,occ_hole,j_a,k_a,other_spin,             &
        !$OMP  hole_save,ispin,jj,l_a,hij_elec,hij_tab,              &
        !$OMP  accu,i_a,hole_tmp,particle_tmp,occ_particle_tmp,      &
        !$OMP  occ_hole_tmp,key_idx,i_b,j_b,key,N_elec_in_key_part_1,&
        !$OMP  N_elec_in_key_hole_1,N_elec_in_key_part_2,            &
        !$OMP  N_elec_in_key_hole_2,ia_ja_pairs)                     &
        !$OMP SHARED(key_in,N_int,elec_num_tab,                      &
        !$OMP  hole_1, particl_1, hole_2, particl_2,                 &
        !$OMP  lck,thresh,elec_alpha_num)"""
    s["omp_init_lock"]    = "call omp_init_lock(lck)"
    s["omp_set_lock"]     = "call omp_set_lock(lck)"
    s["omp_unset_lock"]   = "call omp_unset_lock(lck)"
    s["omp_test_lock"]    = "omp_test_lock(lck)"
    s["omp_destroy_lock"] = "call omp_destroy_lock(lck)"
    s["omp_end_parallel"] = "!$OMP END PARALLEL"
    s["omp_master"]       = "!$OMP MASTER"
    s["omp_end_master"]   = "!$OMP END MASTER"
    s["omp_barrier"]      = "!$OMP BARRIER"
    s["omp_do"]           = "!$OMP DO SCHEDULE (static)"
    s["omp_enddo"]        = "!$OMP ENDDO NOWAIT"

    if not openmp:
      for k in s:
        s[k] = ""
      s["omp_test_lock"]    = ".False."
    s["size_max"] = str(1024*128) 
    s["set_i_H_j_threshold"] = """
      thresh = H_apply_threshold
     """
    self.data = s

  def __setitem__(self,key,value):
    self.data[key] = value

  def __getitem__(self,key):
    return self.data[key]

  def __repr__(self):
    buffer = template
    for key,value in self.data.items():
      buffer = buffer.replace('$'+key, value)
    return buffer

  def set_perturbation(self,pert):
    self.perturbation = pert
    if pert is not None:
      self.data["parameters"] = ",sum_e_2_pert_in,sum_norm_pert_in,sum_H_pert_diag_in,N_st,Nint"
      self.data["declarations"] = """
      integer, intent(in)             :: N_st,Nint
      double precision, intent(inout) :: sum_e_2_pert_in(N_st)
      double precision, intent(inout) :: sum_norm_pert_in(N_st)
      double precision, intent(inout) :: sum_H_pert_diag_in
      double precision                :: sum_e_2_pert(N_st)
      double precision                :: sum_norm_pert(N_st)
      double precision                :: sum_H_pert_diag
      """
      self.data["size_max"] = "256" 
      self.data["initialization"] = """
      E_ref = diag_H_mat_elem(key_in,N_int)
      sum_e_2_pert = sum_e_2_pert_in
      sum_norm_pert = sum_norm_pert_in
      sum_H_pert_diag = sum_H_pert_diag_in
      """
      self.data["keys_work"] += """
      call perturb_buffer_%s(keys_out,key_idx,sum_e_2_pert, &
       sum_norm_pert,sum_H_pert_diag,N_st,Nint)
      """%(pert,)
      self.data["finalization"] = """
      sum_e_2_pert_in = sum_e_2_pert
      sum_norm_pert_in = sum_norm_pert
      sum_H_pert_diag_in = sum_H_pert_diag
      """
      if self.openmp:
        self.data["omp_test_lock"]  = ".False."
        self.data["omp_parallel"]    += """&
 !$OMP SHARED(N_st) &
 !$OMP REDUCTION(+:sum_e_2_pert, sum_norm_pert, sum_H_pert_diag)"""



