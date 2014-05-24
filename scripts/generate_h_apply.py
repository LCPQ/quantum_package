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

    self.selection_pt2 = None
    self.perturbation = None

   #s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(NONE)          &
    s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(SHARED)        &
        !$OMP PRIVATE(i,j,k,l,keys_out,hole,particle,                &
        !$OMP  occ_particle,occ_hole,j_a,k_a,other_spin,             &
        !$OMP  hole_save,ispin,jj,l_a,hij_elec,hij_tab,              &
        !$OMP  accu,i_a,hole_tmp,particle_tmp,occ_particle_tmp,      &
        !$OMP  occ_hole_tmp,key_idx,i_b,j_b,key,N_elec_in_key_part_1,&
        !$OMP  N_elec_in_key_hole_1,N_elec_in_key_part_2,            &
        !$OMP  N_elec_in_key_hole_2,ia_ja_pairs,iproc)               &
        !$OMP SHARED(key_in,N_int,elec_num_tab,mo_tot_num,           &
        !$OMP  hole_1, particl_1, hole_2, particl_2,                 &
        !$OMP  thresh,elec_alpha_num)"""
    s["omp_end_parallel"] = "!$OMP END PARALLEL"
    s["omp_master"]       = "!$OMP MASTER"
    s["omp_end_master"]   = "!$OMP END MASTER"
    s["omp_barrier"]      = "!$OMP BARRIER"
    s["omp_do"]           = "!$OMP DO SCHEDULE (static)"
    s["omp_enddo"]        = "!$OMP ENDDO NOWAIT"

    if not openmp:
      for k in s:
        s[k] = ""
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
    if self.perturbation is not None:
        raise
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
      double precision                :: e_2_pert_buffer(N_st,size_max)
      double precision                :: coef_pert_buffer(N_st,size_max)
      """
      self.data["size_max"] = "256" 
      self.data["initialization"] = """
      sum_e_2_pert = sum_e_2_pert_in
      sum_norm_pert = sum_norm_pert_in
      sum_H_pert_diag = sum_H_pert_diag_in
      PROVIDE reference_energy psi_ref_coef psi_ref
      """
      self.data["keys_work"] += """
      call perturb_buffer_%s(keys_out,key_idx,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert, &
       sum_norm_pert,sum_H_pert_diag,N_st,Nint)
      """%(pert,)
      self.data["finalization"] = """
      sum_e_2_pert_in = sum_e_2_pert
      sum_norm_pert_in = sum_norm_pert
      sum_H_pert_diag_in = sum_H_pert_diag
      """
      if self.openmp:
        self.data["omp_parallel"]    += """&
 !$OMP SHARED(N_st,Nint) PRIVATE(e_2_pert_buffer,coef_pert_buffer) &
 !$OMP REDUCTION(+:sum_e_2_pert, sum_norm_pert, sum_H_pert_diag)"""

  def set_selection_pt2(self,pert):
    if self.selection_pt2 is not None:
        raise
    self.set_perturbation(pert)
    self.selection_pt2 = pert
    if pert is not None:
      self.data["size_max"] = str(1024*128) 
      self.data["keys_work"] = """
      e_2_pert_buffer = 0.d0
      coef_pert_buffer = 0.d0
      """ + self.data["keys_work"]
      self.data["keys_work"] += """
      call fill_H_apply_buffer_selection(key_idx,keys_out,e_2_pert_buffer,coef_pert_buffer,N_st,N_int,iproc)
      """


