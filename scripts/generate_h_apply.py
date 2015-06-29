#!/usr/bin/env python

import os
file = open(os.environ["QP_ROOT"]+'/src/Determinants/H_apply.template.f','r')

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
do_double_excitations
check_double_excitation
copy_buffer
finalization
generate_psi_guess
init_thread
printout_now
printout_always
deinit_thread
skip
init_main
filter_integrals
filter2h2p
filterhole
filterparticle
do_double_excitations
check_double_excitation
""".split()

class H_apply(object):

  def __init__(self,sub,SingleRef=False,do_mono_exc=True, do_double_exc=True):
    s = {}
    for k in keywords:
      s[k] = ""
    s["subroutine"] = "H_apply_%s"%(sub)
    s["params_post"] = ""

    self.selection_pt2 = None
    self.perturbation = None

   #s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(NONE)          &
    s["omp_parallel"]     = """!$OMP PARALLEL DEFAULT(SHARED)        &
        !$OMP PRIVATE(i,j,k,l,keys_out,hole,particle,                &
        !$OMP  occ_particle,occ_hole,j_a,k_a,other_spin,             &
        !$OMP  hole_save,ispin,jj,l_a,ib_jb_pairs,array_pairs,       &
        !$OMP  accu,i_a,hole_tmp,particle_tmp,occ_particle_tmp,      &
        !$OMP  occ_hole_tmp,key_idx,i_b,j_b,key,N_elec_in_key_part_1,&
        !$OMP  N_elec_in_key_hole_1,N_elec_in_key_part_2,            &
        !$OMP  N_elec_in_key_hole_2,ia_ja_pairs)               &
        !$OMP SHARED(key_in,N_int,elec_num_tab,mo_tot_num,           &
        !$OMP  hole_1, particl_1, hole_2, particl_2,                 &
        !$OMP  elec_alpha_num,i_generator) FIRSTPRIVATE(iproc)"""

    s["omp_end_parallel"] = "!$OMP END PARALLEL"
    s["omp_master"]       = "!$OMP MASTER"
    s["omp_end_master"]   = "!$OMP END MASTER"
    s["omp_barrier"]      = "!$OMP BARRIER"
    s["omp_do"]           = "!$OMP DO SCHEDULE (static)"
    s["omp_enddo"]        = "!$OMP ENDDO NOWAIT"

    d = { True : '.True.', False : '.False.'}
    s["do_mono_excitations"] = d[do_mono_exc]
    s["do_double_excitations"] = d[do_double_exc]
    s["keys_work"]  += "call fill_H_apply_buffer_no_selection(key_idx,keys_out,N_int,iproc)"

    s["filter_integrals"] = "array_pairs = .True."
    if SingleRef:
      s["filter_integrals"] = """
      call get_mo_bielec_integrals_existing_ik(i_a,j_a,mo_tot_num,array_pairs,mo_integrals_map)
      """

    s["generate_psi_guess"]  = """
  ! Sort H_jj to find the N_states lowest states
  integer                        :: i
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

    s["size_max"] = str(1024*128) 
    s["copy_buffer"] = """call copy_H_apply_buffer_to_wf
  if (s2_eig) then
    call make_s2_eigenfunction
  endif
  SOFT_TOUCH psi_det psi_coef N_det
"""
    s["printout_now"]   = """write(output_Determinants,*)  &
       100.*float(i_generator)/float(N_det_generators), '% in ', wall_1-wall_0, 's'"""
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

  def unset_double_excitations(self):
    self["do_double_excitations"] = ".False."
    self["check_double_excitation"] = """
     check_double_excitation = .False.
    """
  def set_filter_holes(self):
    self["filterhole"] = """
     if(iand(ibset(0_bit_kind,j),key(k,other_spin)).eq.0_bit_kind )cycle
    """
  def set_filter_particl(self):
    self["filterparticle"] = """
     if(iand(ibset(0_bit_kind,j_a),key(k_a,other_spin)).eq.0_bit_kind )cycle
    """
  def unset_skip(self):
    self["skip"] = """
    """
  def set_filter_2h_2p(self):
    self["filter2h2p"] = """
!    ! DIR$ FORCEINLINE
!   call get_excitation_degree(ref_bitmask,key,degree,N_int)
!   if(degree==1)then
!    print*,'degree = ',degree
!   endif
     if(is_a_two_holes_two_particles(key))cycle
    """


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
      double precision, intent(inout) :: sum_H_pert_diag_in(N_st)
      double precision                :: sum_e_2_pert(N_st)
      double precision                :: sum_norm_pert(N_st)
      double precision                :: sum_H_pert_diag(N_st)
      double precision, allocatable   :: e_2_pert_buffer(:,:)
      double precision, allocatable   :: coef_pert_buffer(:,:)
      ASSERT (Nint == N_int)
      """
      self.data["init_thread"] = """
      allocate (e_2_pert_buffer(N_st,size_max), coef_pert_buffer(N_st,size_max))
      do k=1,N_st
        sum_e_2_pert(k) = 0.d0
        sum_norm_pert(k) = 0.d0
        sum_H_pert_diag(k) = 0.d0
      enddo
      """ 

      self.data["deinit_thread"] = """
      !$ call omp_set_lock(lck)
      do k=1,N_st
        sum_e_2_pert_in(k) = sum_e_2_pert_in(k) + sum_e_2_pert(k)
        sum_norm_pert_in(k) = sum_norm_pert_in(k) + sum_norm_pert(k)
        sum_H_pert_diag_in(k) = sum_H_pert_diag_in(k) + sum_H_pert_diag(k)
      enddo
      !$ call omp_unset_lock(lck)
      deallocate (e_2_pert_buffer, coef_pert_buffer)
      """
      self.data["size_max"] = "256" 
      self.data["initialization"] = """
      PROVIDE CI_electronic_energy psi_selectors_coef psi_selectors E_corr_per_selectors psi_det_sorted_bit
      """
      self.data["keys_work"] = """
      if(check_double_excitation)then
       call perturb_buffer_%s(i_generator,keys_out,key_idx,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert, &
        sum_norm_pert,sum_H_pert_diag,N_st,N_int)
      else 
       call perturb_buffer_by_mono_%s(i_generator,keys_out,key_idx,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert, &
        sum_norm_pert,sum_H_pert_diag,N_st,N_int)
      endif
      """%(pert,pert)
      self.data["finalization"] = """
      """
      self.data["copy_buffer"] = ""
      self.data["generate_psi_guess"] = ""

      self.data["params_main"] = "pt2, norm_pert, H_pert_diag, N_st"
      self.data["params_post"] = ","+self.data["params_main"] +", N_int"
      self.data["decls_main"] = """  integer, intent(in)            :: N_st 
  double precision, intent(inout):: pt2(N_st) 
  double precision, intent(inout):: norm_pert(N_st) 
  double precision, intent(inout):: H_pert_diag(N_st)
  double precision               :: delta_pt2(N_st), norm_psi(N_st), pt2_old(N_st)
  PROVIDE CI_electronic_energy N_det_generators 
  do k=1,N_st
    pt2(k) = 0.d0
    norm_pert(k) = 0.d0
    H_pert_diag(k) = 0.d0
    norm_psi(k) = 0.d0
    delta_pt2(k) = 0.d0
    pt2_old(k) = 0.d0
  enddo
        write(output_Determinants,'(A12, X, A8, 3(2X, A9), 2X, A8, 2X, A8, 2X, A8)') &
                 'N_generators', 'Norm', 'Delta PT2', 'PT2', 'Est. PT2', 'secs'
        write(output_Determinants,'(A12, X, A8, 3(2X, A9), 2X, A8, 2X, A8, 2X, A8)') &
                 '============', '========', '=========', '=========', '=========', &
                 '========='
      """ 

      self.data["printout_always"] = """
      do k=1,N_st
          norm_psi(k) = norm_psi(k) + psi_coef_generators(i_generator,k)*psi_coef_generators(i_generator,k)
        delta_pt2(k) = pt2(k) - pt2_old(k)
      enddo
      """
      self.data["printout_now"] = """
      do k=1,N_st
        write(output_Determinants,'(I10, 4(2X, F9.6), 2X, F8.1)') &
                 i_generator, norm_psi(k), delta_pt2(k), pt2(k), &
                 pt2(k)/(norm_psi(k)*norm_psi(k)), &
                 wall_1-wall_0
         pt2_old(k) = pt2(k)
      enddo
      progress_value = norm_psi(1)
      """
      self.data["omp_parallel"]    += """&
 !$OMP SHARED(N_st) PRIVATE(e_2_pert_buffer,coef_pert_buffer) &
 !$OMP PRIVATE(sum_e_2_pert, sum_norm_pert, sum_H_pert_diag)"""

  def set_selection_pt2(self,pert):
    if self.selection_pt2 is not None:
        raise
    self.set_perturbation(pert)
    self.selection_pt2 = pert
    if pert is not None:
      self.data["parameters"] += ",select_max_out"
      self.data["declarations"] += """
      double precision, intent(inout) :: select_max_out"""

      self.data["params_post"] += ", select_max(min(i_generator,size(select_max,1)))"
      self.data["size_max"] = str(1024*128) 
      self.data["copy_buffer"] = """
      call copy_H_apply_buffer_to_wf
      if (s2_eig) then
        call make_s2_eigenfunction
      endif
!      SOFT_TOUCH psi_det psi_coef N_det
      selection_criterion_min = min(selection_criterion_min, maxval(select_max))*0.1d0
      selection_criterion = selection_criterion_min
      call write_double(output_Determinants,selection_criterion,'Selection criterion')
      """
      self.data["keys_work"] = """
      e_2_pert_buffer = 0.d0
      coef_pert_buffer = 0.d0
      """ + self.data["keys_work"]
      self.data["keys_work"] += """
      call fill_H_apply_buffer_selection(key_idx,keys_out,e_2_pert_buffer, &
        coef_pert_buffer,N_st,N_int,iproc,select_max_out) 
      """
      self.data["omp_parallel"]    += """&
 !$OMP REDUCTION (max:select_max_out)"""
      self.data["skip"] = """
      if (i_generator < size_select_max) then
        if (select_max(i_generator) < selection_criterion_min*selection_criterion_factor) then
          !$ call omp_set_lock(lck)
          do k=1,N_st
             norm_psi(k) = norm_psi(k) + psi_coef_generators(i_generator,k)*psi_coef_generators(i_generator,k)
             delta_pt2(k) = 0.d0
             pt2_old(k) = 0.d0
             pt2(k) = select_max(i_generator)
          enddo
          !$ call omp_unset_lock(lck)
          cycle
        endif
        select_max(i_generator) = 0.d0
      endif
      """


