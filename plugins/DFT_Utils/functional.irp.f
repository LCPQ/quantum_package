subroutine ex_lda(rho_a,rho_b,ex,vx_a,vx_b)
 include 'constants.include.F'
 implicit none
 double precision, intent(in) :: rho_a,rho_b
 double precision, intent(out) :: ex,vx_a,vx_b
 double precision :: tmp_a,tmp_b
 tmp_a = rho_a**(c_1_3)
 tmp_b = rho_b**(c_1_3)
 ex = cst_lda * (tmp_a*tmp_a*tmp_a*tmp_a + tmp_b*tmp_b*tmp_b*tmp_b)
 vx_a = cst_lda * c_4_3 * tmp_a
 vx_b = cst_lda * c_4_3 * tmp_b

end

 BEGIN_PROVIDER [double precision, lda_exchange, (N_states)]
&BEGIN_PROVIDER [double precision, lda_ex_potential_alpha_ao,(ao_num_align,ao_num,N_states)]
&BEGIN_PROVIDER [double precision, lda_ex_potential_beta_ao,(ao_num_align,ao_num,N_states)]

 implicit none
 integer :: i,j,k,l
 integer :: m,n
 double precision :: aos_array(ao_num)
 double precision :: r(3)
 lda_ex_potential_alpha_ao = 0.d0
 lda_ex_potential_beta_ao = 0.d0
 do l = 1, N_states
  lda_exchange(l) = 0.d0
  do j = 1, nucl_num
   do i = 1, n_points_radial_grid 
    do k = 1, n_points_integration_angular
     double precision :: rho_a,rho_b,ex
     double precision :: vx_a,vx_b
     rho_a = one_body_dm_mo_alpha_at_grid_points(k,i,j,l)
     rho_b = one_body_dm_mo_beta_at_grid_points(k,i,j,l)
     call ex_lda(rho_a,rho_b,ex,vx_a,vx_b) 
     lda_exchange(l) += final_weight_functions_at_grid_points(k,i,j) * ex
     r(1) = grid_points_per_atom(1,k,i,j) 
     r(2) = grid_points_per_atom(2,k,i,j) 
     r(3) = grid_points_per_atom(3,k,i,j) 
     call give_all_aos_at_r(r,aos_array)
     do m = 1, ao_num
!     lda_ex_potential_ao(m,m,l) += (vx_a + vx_b) * aos_array(m)*aos_array(m)
      do n = 1, ao_num
       lda_ex_potential_alpha_ao(m,n,l) += (vx_a ) * aos_array(m)*aos_array(n) * final_weight_functions_at_grid_points(k,i,j)
       lda_ex_potential_beta_ao(m,n,l) += (vx_b) * aos_array(m)*aos_array(n) * final_weight_functions_at_grid_points(k,i,j)
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 

