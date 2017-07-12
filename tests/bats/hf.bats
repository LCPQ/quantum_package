#!/usr/bin/env bats

source $QP_ROOT/tests/bats/common.bats.sh

function run_init() {
  cp "${QP_ROOT}/tests/input/$1" .
  qp_create_ezfio_from_xyz $1 -o $3 $2
  qp_edit -c $3
}


function run_HF() {
  thresh=1.e-7
  test_exe SCF || skip
  qp_edit -c $1
  ezfio set_file $1
  ezfio set hartree_fock thresh_scf 2.e-8
  qp_run SCF $1
  energy="$(ezfio get hartree_fock energy)"
  eq $energy $2 $thresh
}



#=== DHNO
@test "init DHNO chipman-dzp" {
  run_init dhno.xyz "-b chipman-dzp -m 2" dhno.ezfio
}

@test "SCF DHNO chipman-dzp" {
  run_HF  dhno.ezfio  -130.4278777822   
}

#=== HBO
@test "init HBO STO-3G" {
  run_init HBO.xyz "-b STO-3G" hbo.ezfio
}

@test "SCF HBO STO-3G" {
  run_HF  hbo.ezfio  -98.8251985678084 
}


#=== H2O
@test "init H2O cc-pVDZ" {
  run_init h2o.xyz "-b cc-pvdz" h2o.ezfio
}

@test "SCF H2O cc-pVDZ" {
  run_HF  h2o.ezfio  -0.760270218692179E+02
}

