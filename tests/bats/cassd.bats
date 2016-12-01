#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

@test "CAS_SD H2O cc-pVDZ" {
  test_exe cassd_zmq || skip
  INPUT=h2o.ezfio
  rm -rf work/h2o.ezfio/determinants/
  qp_edit -c $INPUT
  ezfio set_file $INPUT
  ezfio set perturbation do_pt2_end True 
  ezfio set determinants n_det_max 16384
  qp_set_mo_class $INPUT -core "[1]" -inact "[2,5]" -act "[3,4,6,7]" -virt "[8-24]" 
  qp_run cassd_zmq $INPUT  
  energy="$(ezfio get cas_sd_zmq energy_pt2)"
  eq $energy -76.2311422169983 5.E-5

  ezfio set determinants n_det_max 2048 
  ezfio set determinants read_wf True
  ezfio set perturbation do_pt2_end True 
  qp_run cassd_zmq $INPUT  
  ezfio set determinants read_wf False
  energy="$(ezfio get cas_sd_zmq energy)"
  eq $energy -76.2300888408526   2.E-5
}

