#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

#=== H2O
@test "MRCC-lambda H2O cc-pVDZ" {
  INPUT=h2o.ezfio
  EXE=mrcc
  test_exe $EXE || skip
  qp_edit -c $INPUT  
  ezfio set_file $INPUT
  ezfio set determinants threshold_generators 1.
  ezfio set determinants threshold_selectors  1.
  ezfio set determinants read_wf True
  ezfio set mrcepa0 lambda_type 1
  ezfio set mrcepa0 n_it_max_dressed_ci 3
  cp -r $INPUT TMP ; qp_run $EXE TMP 
  ezfio set_file TMP
  energy="$(ezfio get mrcepa0 energy_pt2)"
  rm -rf TMP
  eq $energy -76.2382119593927 1.e-4
}

@test "MRCC H2O cc-pVDZ" {
  INPUT=h2o.ezfio
  EXE=mrcc
  test_exe $EXE || skip
  qp_edit -c $INPUT  
  ezfio set_file $INPUT
  ezfio set determinants threshold_generators 1.
  ezfio set determinants threshold_selectors  1.
  ezfio set determinants read_wf True
  ezfio set mrcepa0 lambda_type 0
  ezfio set mrcepa0 n_it_max_dressed_ci 3
  cp -r $INPUT TMP ; qp_run $EXE TMP 
  ezfio set_file TMP
  energy="$(ezfio get mrcepa0 energy_pt2)"
  rm -rf TMP
  eq $energy -76.2381753982902   1.e-4
}

@test "MRSC2 H2O cc-pVDZ" {
  INPUT=h2o.ezfio
  EXE=mrsc2
  test_exe $EXE || skip
  qp_edit -c $INPUT  
  ezfio set_file $INPUT
  ezfio set determinants threshold_generators 1.
  ezfio set determinants threshold_selectors  1.
  ezfio set determinants read_wf True
  ezfio set mrcepa0 lambda_type 1
  ezfio set mrcepa0 n_it_max_dressed_ci 3
  cp -r $INPUT TMP ; qp_run $EXE TMP 
  ezfio set_file TMP
  energy="$(ezfio get mrcepa0 energy_pt2)"
  rm -rf TMP
  eq $energy -76.2359960472962 3.e-4
}

@test "MRCEPA0 H2O cc-pVDZ" {
  INPUT=h2o.ezfio
  EXE=mrcepa0
  test_exe $EXE || skip
  qp_edit -c $INPUT  
  ezfio set_file $INPUT
  ezfio set determinants threshold_generators 1.
  ezfio set determinants threshold_selectors  1.
  ezfio set determinants read_wf True
  ezfio set mrcepa0 lambda_type 1
  ezfio set mrcepa0 n_it_max_dressed_ci 3
  cp -r $INPUT TMP ; qp_run $EXE TMP
  ezfio set_file TMP
  energy="$(ezfio get mrcepa0 energy_pt2)"
  rm -rf TMP
  eq $energy -76.2411825032868 2.e-4
}

