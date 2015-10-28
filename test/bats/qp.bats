#!/usr/bin/env bats

# float number comparison
# Compare two number ($1, $2) with a given precision ($3)
# If the number are not equal, the exit is 1 else is 0

# So we strip the "-", is the abs value of the poor
function eq() {
    awk -v n1=${1#-} -v n2=${2#-} -v p=$3 'BEGIN{ if ((n1-n2)^2 < p^2) exit 0; exit 1}'
}

#: "${QP_ROOT?Pls set your quantum_package.rc}"

source ${QP_ROOT}/install/EZFIO/Bash/ezfio.sh
TEST_DIR=${QP_ROOT}/test/work/
mkdir -p ${TEST_DIR}
cd ${TEST_DIR}

@test "init HBO STO-3G" {
  cp ${QP_ROOT}/test/input/HBO.xyz .
  qp_create_ezfio_from_xyz -b "STO-3G" HBO.xyz
  qp_edit -c HBO.ezfio
}


@test "hartree fock HBO STO-3G" {
  run init HBO STO-3G
  qp_run SCF HBO.ezfio
  # Check energy
  ezfio set_file HBO.ezfio
  energy="$(ezfio get hartree_fock energy)"
  eq $energy -98.82519856228865 1E-6
}

@test "full ci HBO STO-3G" {
  run init HBO STO-3G

  ezfio set_file HBO.ezfio
  ezfio set perturbation do_pt2_end 1
  ezfio set determinants n_det_max 10000

  qp_run full_ci HBO.ezfio
  energy="$(ezfio get full_ci energy)"
  eq $energy -98.9661013229156 1E-6
  energy_pt2="$(ezfio get full_ci energy_pt2)"
  eq $energy_pt2 -98.966293393389 1E-6
}


@test "cas_sd_selected HBO STO-3G" {
  run hartree fock HBO STO-3G
  ezfio set_file HBO.ezfio
  ezfio set perturbation do_pt2_end 0
  ezfio set determinants n_det_max 1000
  qp_set_mo_class HBO.ezfio  -core "[1-2]" -inact "[3-5]" -act "[6-9]" -virt "[10-11]"
  qp_run cas_sd_selected HBO.ezfio
  # Check energy
  energy="$(ezfio get cas_sd energy)"
  eq $energy -98.9640982255169 1E-6
}

@test "mrcc_cassd HBO STO-3G" {
  run cas_sd_selected fock HBO STO-3G
  ezfio set_file HBO.ezfio
  ezfio set determinants threshold_generators 1
  ezfio set determinants read_wf 1
  qp_run mrcc_cassd HBO.ezfio
  # Check energy
  energy="$(ezfio get mrcc_cassd energy)"
  eq $energy -98.9647967033634 1E-6
}

@test "script conversion HBO.out" {
  cp ${QP_ROOT}/test/input/HBO.out .
  qp_convert_output_to_ezfio.py HBO.out
  qp_edit -c HBO.out.ezfio
  qp_run SCF HBO.out.ezfio
  ezfio set_file HBO.out.ezfio
  energy="$(ezfio get hartree_fock energy)"
  eq $energy -100.0185822553404 1E-6
}
