#!/usr/bin/env python

import os
file = open(os.environ["QPACKAGE_ROOT"]+'/src/Dets/H_apply_template.f','r')
template = file.read()
file.close()

keywords = """
subroutine
parameters
params_main
initialization
declarations
decls_main
keys_work
copy_buffer
finalization
generate_psi_guess
""".split()

class H_apply(object):

  def __init__(self,sub,openmp=True):
    s = {}
    for k in keywords:
      s[k] = ""
    s["subroutine"] = "H_apply_%s"%(sub)
    s["params_post"] = ""
    self.openmp = openmp

    self.selection_pt2 = None
    self.perturbation = None

   #s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(NONE)          &
    s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(SHARED)        &
        !$OMP PRIVATE(i,j,k,l,keys_out,hole,particle,                &
        !$OMP  occ_particle,occ_hole,j_a,k_a,other_spin,             &
        !$OMP  hole_save,ispin,jj,l_a,                               &
        !$OMP  accu,i_a,hole_tmp,particle_tmp,occ_particle_tmp,      &
        !$OMP  occ_hole_tmp,key_idx,i_b,j_b,key,N_elec_in_key_part_1,&
        !$OMP  N_elec_in_key_hole_1,N_elec_in_key_part_2,            &
        !$OMP  N_elec_in_key_hole_2,ia_ja_pairs,iproc)               &
        !$OMP SHARED(key_in,N_int,elec_num_tab,mo_tot_num,           &
        !$OMP  hole_1, particl_1, hole_2, particl_2,                 &
        !$OMP  thresh,elec_alpha_num,i_generator)"""
    s["omp_end_parallel"] = "!$OMP END PARALLEL"
    s["omp_master"]       = "!$OMP MASTER"
    s["omp_end_master"]   = "!$OMP END MASTER"
    s["omp_barrier"]      = "!$OMP BARRIER"
    s["omp_do"]           = "!$OMP DO SCHEDULE (static)"
    s["omp_enddo"]        = "!$OMP ENDDO NOWAIT"

    s["keys_work"]  += "call fill_H_apply_buffer_no_selection(key_idx,keys_out,N_int,iproc)"

    s["generate_psi_guess"]  = """
  ! Sort H_jj to find the N_states lowest states
  integer                        :: i,k
  integer, allocatable           :: iorder(:)
  double precision, allocatable  :: H_jj(:)
  double precision, external     :: diag_h_mat_elem
  allocate(H_jj(N_det),iorder(N_det))
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP SHARED(psi_det,N_int,H_jj,iorder,N_det)                  &
      !$OMP PRIVATE(i)
  !$OMP DO
  do i = 1, N_det
    H_jj(i) = diag_h_mat_elem(psi_det(1,1,i),N_int)
    iorder(i) = i
  enddo
  !$OMP END DO
  !$OMP END PARALLEL

  call dsort(H_jj,iorder,N_det)
  do k=1,N_states
    psi_coef(iorder(k),k) = 1.d0
  enddo
  deallocate(H_jj,iorder)
    """

    if not openmp:
      for k in s:
        s[k] = ""
    s["size_max"] = str(1024*128) 
    s["set_i_H_j_threshold"] = """
      thresh = H_apply_threshold
     """
    s["copy_buffer"] = "call copy_h_apply_buffer_to_wf"
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
      PROVIDE CI_electronic_energy psi_selectors_coef psi_selectors
      """
      self.data["keys_work"] = """
      call perturb_buffer_%s(i_generator,keys_out,key_idx,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert, &
       sum_norm_pert,sum_H_pert_diag,N_st,Nint)
      """%(pert,)
      self.data["finalization"] = """
      sum_e_2_pert_in = sum_e_2_pert
      sum_norm_pert_in = sum_norm_pert
      sum_H_pert_diag_in = sum_H_pert_diag
      """
      self.data["copy_buffer"] = ""
      self.data["generate_psi_guess"] = ""

      self.data["params_main"] = "pt2, norm_pert, H_pert_diag, N_st"
      self.data["params_post"] = ","+self.data["params_main"] +", N_int"
      self.data["decls_main"] = """  integer, intent(in)            :: N_st 
  double precision, intent(inout):: pt2(N_st) 
  double precision, intent(inout):: norm_pert(N_st) 
  double precision, intent(inout):: H_pert_diag
  PROVIDE CI_electronic_energy N_det_generators key_pattern_not_in_ref                                                                                                      
  pt2 = 0.d0
  norm_pert = 0.d0
  H_pert_diag = 0.d0
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
      self.data["copy_buffer"] = """
      call copy_h_apply_buffer_to_wf
      selection_criterion_min = selection_criterion_min*0.1d0
      selection_criterion = selection_criterion_min
      call remove_small_contributions
      """
      self.data["keys_work"] = """
      e_2_pert_buffer = 0.d0
      coef_pert_buffer = 0.d0
      """ + self.data["keys_work"]
      self.data["keys_work"] += """
      call fill_H_apply_buffer_selection(key_idx,keys_out,e_2_pert_buffer,coef_pert_buffer,N_st,N_int,iproc)
      """


