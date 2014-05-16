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

def new_dict(openmp=True):
    s ={}
    for k in keywords:
      s[k] = ""
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
        !$OMP  lck,thresh,elec_alpha_num,E_ref)"""
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
    return s



def create_h_apply(s):
    buffer = template
    for key in s:
      buffer = buffer.replace('$'+key, s[key])
    print buffer




