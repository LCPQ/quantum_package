BEGIN_PROVIDER [double precision, mo_general_density_alpha, (mo_tot_num_align,mo_tot_num)]
 implicit none
 integer  :: i,j,k,l
 mo_general_density_alpha =  one_body_dm_mo_alpha_generators_restart

END_PROVIDER


BEGIN_PROVIDER [double precision, mo_general_density_beta, (mo_tot_num_align,mo_tot_num)]
 implicit none
 integer  :: i,j,k,l
 mo_general_density_beta =  one_body_dm_mo_beta_generators_restart

END_PROVIDER


