program test
  implicit none
  integer :: i,j,k,l
  double precision :: accu,accu2,get_mo_bielec_integral,accu3
  accu = 0.d0
  accu3 = 0.d0
  do i = 1, elec_beta_num
   do j = 1, elec_beta_num
     accu +=  (2.d0 * get_mo_bielec_integral(i,j,i,j,mo_integrals_map) - get_mo_bielec_integral(i,j,j,i,mo_integrals_map) )
     accu3 += (2.d0 * mo_bielec_integral_jj(i,j) - mo_bielec_integral_jj_exchange(i,j) )
   enddo
  enddo
  print *,'closed EE 1 : ',accu
  print *,'closed EE 2 : ',accu3 
end
