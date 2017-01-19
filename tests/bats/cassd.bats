#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

@test "CAS_SD H2O cc-pVDZ" {
  test_exe cas_sd_selected || skip
  INPUT=h2o.ezfio
  qp_edit -c $INPUT
  ezfio set_file $INPUT
  ezfio set perturbation do_pt2_end False
  ezfio set determinants n_det_max 1000
  qp_set_mo_class $INPUT -core "[1]" -inact "[2,5]" -act "[3,4,6,7]" -virt "[8-24]" 
  qp_run cas_sd_selected $INPUT  
  energy="$(ezfio get cas_sd energy)"
  eq $energy -76.22213389282479 1.E-5
}

