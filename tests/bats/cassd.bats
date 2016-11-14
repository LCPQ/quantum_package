#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

@test "CAS_SD H2O cc-pVDZ" {
  test_exe cassd_zmq || skip
  INPUT=h2o.ezfio
  qp_edit -c $INPUT
  ezfio set_file $INPUT
  ezfio set perturbation do_pt2_end False
  ezfio set determinants n_det_max 1000
  qp_set_mo_class $INPUT -core "[1]" -inact "[2,5]" -act "[3,4,6,7]" -virt "[8-24]" 
  qp_run cassd_zmq $INPUT  
  energy="$(ezfio get cas_sd_zmq energy)"
  eq $energy -76.2221338928418   1.E-5
}

