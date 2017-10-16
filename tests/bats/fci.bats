#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

function run_FCI() {
  thresh=5.e-5
  test_exe full_ci || skip
  qp_edit -c $1
  ezfio set_file $1
  ezfio set perturbation do_pt2 True
  ezfio set determinants n_det_max $2
  ezfio set davidson threshold_davidson 1.e-10

  qp_run full_ci $1
  energy="$(ezfio get full_ci energy)"
  eq $energy $3 $thresh
  energy_pt2="$(ezfio get full_ci energy_pt2)"
  eq $energy_pt2 $4 $thresh
}

function run_FCI_ZMQ() {
  thresh=5.e-5
  test_exe fci_zmq || skip
  qp_edit -c $1
  ezfio set_file $1
  ezfio set perturbation do_pt2 True
  ezfio set determinants n_det_max $2
  ezfio set davidson threshold_davidson 1.e-10

  qp_run fci_zmq $1
  energy="$(ezfio get full_ci_zmq energy)"
  eq $energy $3 $thresh
  energy_pt2="$(ezfio get full_ci_zmq energy_pt2)"
  eq $energy_pt2 $4 $thresh
}



#=== H2O

@test "qp_set_mo_class H2O cc-pVDZ" {
  qp_set_mo_class h2o.ezfio -core "[1]" -act "[2-12]" -del "[13-24]"
}
@test "FCI H2O cc-pVDZ" {
  run_FCI h2o.ezfio 2000  -76.1253757275131     -76.1258128174355
}




@test "FCI-ZMQ H2O cc-pVDZ" {
  run_FCI_ZMQ h2o.ezfio 2000 -76.1250552686394     -76.1258817228809  
}


