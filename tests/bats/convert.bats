#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

#=== Convert
@test "gamess convert HBO.out" {
  cp ${QP_ROOT}/tests/input/HBO.out .
  qp_convert_output_to_ezfio.py HBO.out
  qp_edit -c HBO.out.ezfio 
  ezfio set_file HBO.out.ezfio
  qp_run SCF HBO.out.ezfio 
  # Check energy
  energy="$(ezfio get hartree_fock energy)"
  eq $energy -100.0185822590964 1.e-10
}

@test "g09 convert H2O.log" {
  cp ${QP_ROOT}/tests/input/h2o.log .
  qp_convert_output_to_ezfio.py h2o.log
  qp_edit -c h2o.log.ezfio 
  ezfio set_file h2o.log.ezfio
  qp_run SCF h2o.log.ezfio 
  # Check energy
  energy="$(ezfio get hartree_fock energy)"
  eq $energy -76.0270218704265 1E-10
}

