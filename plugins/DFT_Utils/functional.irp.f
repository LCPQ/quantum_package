double precision function ex_lda(rho)
 include 'constants.include.F'
 implicit none
 double precision, intent(in) :: rho
 ex_lda = cst_lda * rho**(c_4_3)

end

BEGIN_PROVIDER [double precision, lda_exchange, (N_states)]
 implicit none
 integer :: i,j,k,l
 double precision :: ex_lda
 do l = 1, N_states
  lda_exchange(l) = 0.d0
  do j = 1, nucl_num
   do i = 1, n_points_radial_grid 
    do k = 1, n_points_integration_angular
     lda_exchange(l) += final_weight_functions_at_grid_points(k,i,j) * &
     (ex_lda(one_body_dm_mo_alpha_at_grid_points(k,i,j,l)) + ex_lda(one_body_dm_mo_beta_at_grid_points(k,i,j,l))) 
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 
