BEGIN_PROVIDER [ logical, all_ao_integrals ]
 implicit none
 BEGIN_DOC
 ! Forces all provieders of AO integrals to be provided
 END_DOC

  PROVIDE all_utils nucl_coord ao_overlap_abs ao_overlap
END_PROVIDER

BEGIN_PROVIDER [ logical, all_mo_integrals ]
 implicit none
 BEGIN_DOC
 ! Forces all provieders of MO integrals to be provided
 END_DOC

  PROVIDE all_utils nucl_coord mo_bielec_integral_jj
  PROVIDE mo_bielec_integral_jj_exchange mo_bielec_integral_jj_anti
END_PROVIDER

