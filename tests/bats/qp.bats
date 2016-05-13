#!/usr/bin/env bats

#                
#   |\/| o  _  _ 
#   |  | | _> (_ 
#                
# floating point number comparison
# Compare two numbers ($1, $2) with a given precision ($3)
# If the numbers are not equal, the exit code is 1 else it is 0
# So we strip the "-", is the abs value of the poor
function eq() {
    declare -a diff
    diff=($(awk -v d1=$1 -v d2=$2 -v n1=${1#-} -v n2=${2#-} -v p=$3 'BEGIN{ if ((n1-n2)^2 < p^2) print 0; print 1 " " (d1-d2) " " d1 " " d2 }'))
    if [[ "${diff[0]}" == "0" ]]
    then
       return 0
    else
       echo "Test      : " ${BATS_TEST_DESCRIPTION}
       echo "Error     : " ${diff[1]}
       echo "Reference : " ${diff[3]}
       echo "Computed  : " ${diff[2]}
       exit 1
    fi
}


#   ___           
#    |  ._  o _|_ 
#   _|_ | | |  |_ 
#                 
source ${QP_ROOT}/install/EZFIO/Bash/ezfio.sh
TEST_DIR=${QP_ROOT}/tests/work/

mkdir -p "${TEST_DIR}"

cd "${TEST_DIR}" || exit 1

function run_init() {
  cp "${QP_ROOT}/tests/input/$1" .
  qp_create_ezfio_from_xyz $1 -o $3 $2 
  qp_edit -c $3
}

function test_exe() {
  EXE=$(awk "/^$1 / { print \$2 }" < "${QP_ROOT}"/data/executables)
  EXE=$(echo $EXE | sed "s|\$QP_ROOT|$QP_ROOT|")
  if [[ -x "$EXE" ]]
  then
    return 0
  else
    return 127
  fi
}

function run_HF() {
  thresh=1.e-7
  test_exe SCF || skip
  ezfio set_file $1
  ezfio set hartree_fock thresh_scf 1.e-11
  qp_run SCF $1 
  energy="$(ezfio get hartree_fock energy)"
  eq $energy $2 $thresh
}

function run_FCI() {
  thresh=5.e-5
  test_exe full_ci || skip
  ezfio set_file $1
  ezfio set perturbation do_pt2_end True
  ezfio set determinants n_det_max $2
  ezfio set determinants threshold_davidson 1.e-10

  qp_run full_ci $1 
  energy="$(ezfio get full_ci energy)"
  eq $energy $3 $thresh
  energy_pt2="$(ezfio get full_ci energy_pt2)"
  eq $energy_pt2 $4 $thresh
}

function run_all_1h_1p() {
  thresh=1.e-6
  test_exe all_1h_1p || skip
  ezfio set_file $1
  ezfio set determinants n_det_max $2
  ezfio set perturbation pt2_max $3
  ezfio set determinants threshold_davidson 1.e-10

  qp_run all_1h_1p $1 | tee $1.F1h1p.out
  energy="$(ezfio get all_singles energy)"
  eq $energy $4 $thresh
}

#   ___             
#    |  _   _ _|_   
#    | (/_ _>  |_   
#                   


#=== DHNO
@test "init DHNO chipman-dzp" {
  run_init dhno.xyz "-b chipman-dzp -m 2" dhno.ezfio
}

@test "SCF DHNO chipman-dzp" {
  run_HF  dhno.ezfio  -130.4278777822   
}

@test "all_1h_1p DHNO chipman-dzp" {
  qp_set_mo_class -inact "[1-8]" -act "[9]" -virt "[10-64]" dhno.ezfio 
  run_all_1h_1p dhno.ezfio 10000 0.0000000001  -130.4466283766202
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

@test "FCI H2O cc-pVDZ" {
  qp_set_mo_class h2o.ezfio -core "[1]" -act "[2-12]" -del "[13-24]"
  run_FCI h2o.ezfio 2000  -0.761255633582109E+02 -0.761258377850042E+02
}

@test "CAS_SD H2O cc-pVDZ" {
  test_exe cas_sd_selected || skip
  INPUT=h2o.ezfio
  ezfio set_file $INPUT
  ezfio set perturbation do_pt2_end False
  ezfio set determinants n_det_max 1000
  qp_set_mo_class $INPUT -core "[1]" -inact "[2,5]" -act "[3,4,6,7]" -virt "[8-24]" 
  qp_run cas_sd_selected $INPUT  
  energy="$(ezfio get cas_sd energy)"
  eq $energy -0.762219854008117E+02 1.E-5
}

@test "MRCC H2O cc-pVDZ" {
  test_exe mrcc_cassd || skip
  INPUT=h2o.ezfio
  ezfio set_file $INPUT
  ezfio set determinants threshold_generators 1.
  ezfio set determinants threshold_selectors  1.
  ezfio set determinants read_wf True
  qp_run mrcc_cassd $INPUT  
  energy="$(ezfio get mrcc_cassd energy)"
  eq $energy -76.2288648023833  1.e-4
  
}


#=== H2O Pseudo
@test "init H2O VDZ pseudo" {
  run_init h2o.xyz "-p bfd -b vdz-bfd" h2o_pseudo.ezfio
}

@test "SCF H2O VDZ pseudo" {
  run_HF  h2o_pseudo.ezfio  -16.9483703905461
}

@test "FCI H2O VDZ pseudo" {
  qp_set_mo_class h2o_pseudo.ezfio -core "[1]" -act "[2-12]" -del "[13-23]"
  run_FCI h2o_pseudo.ezfio 2000    -0.170399597228904E+02 -0.170400168816800E+02
}

#=== Convert
@test "gamess convert HBO.out" {
  cp ${QP_ROOT}/tests/input/HBO.out .
  qp_convert_output_to_ezfio.py HBO.out
  ezfio set_file HBO.out.ezfio
  qp_run SCF HBO.out.ezfio 
  # Check energy
  energy="$(ezfio get hartree_fock energy)"
  eq $energy -100.0185822590964 1.e-10
}

@test "g09 convert H2O.log" {
  cp ${QP_ROOT}/tests/input/h2o.log .
  qp_convert_output_to_ezfio.py h2o.log
  ezfio set_file h2o.log.ezfio
  qp_run SCF h2o.log.ezfio 
  # Check energy
  energy="$(ezfio get hartree_fock energy)"
  eq $energy -76.0270218704265 1E-10
}


# TODO N_int = 1,2,3,4,5
# TODO mod(64) MOs
# TODO All G2 SCF energies
# TODO Long and short tests
# TODO MP2
# TODO CISD_selected

