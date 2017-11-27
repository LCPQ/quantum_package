 BEGIN_PROVIDER [double precision, big_array_coulomb_integrals, (mo_tot_num,mo_tot_num, mo_tot_num)]
&BEGIN_PROVIDER [double precision, big_array_exchange_integrals,(mo_tot_num,mo_tot_num, mo_tot_num)]
 implicit none
 integer :: i,j,k,l
 double precision :: get_mo_bielec_integral
 double precision :: integral

 do k = 1, mo_tot_num
  do i = 1, mo_tot_num
   do j = 1, mo_tot_num
     l = j
     integral = get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     big_array_coulomb_integrals(j,i,k) = integral
     l = j
     integral = get_mo_bielec_integral(i,j,l,k,mo_integrals_map)
     big_array_exchange_integrals(j,i,k) = integral
   enddo
  enddo
 enddo


END_PROVIDER 
