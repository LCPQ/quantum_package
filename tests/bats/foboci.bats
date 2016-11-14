#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

function run_all_1h_1p() {
  thresh=1.e-6
  test_exe all_1h_1p || skip
  qp_edit -c $1
  ezfio set_file $1
  ezfio set determinants n_det_max $2
  ezfio set perturbation pt2_max $3
  ezfio set davidson threshold_davidson 1.e-10

  qp_run all_1h_1p $1 | tee $1.F1h1p.out
  energy="$(ezfio get all_singles energy)"
  eq $energy $4 $thresh
}


#=== DHNO

@test "all_1h_1p DHNO chipman-dzp" {
  qp_set_mo_class -inact "[1-8]" -act "[9]" -virt "[10-64]" dhno.ezfio 
  run_all_1h_1p dhno.ezfio 10000 0.0000000001  -130.4466283766202
}


