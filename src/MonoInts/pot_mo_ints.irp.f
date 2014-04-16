BEGIN_PROVIDER [double precision, mo_nucl_elec_integral, (mo_tot_num_align,mo_tot_num)]
 implicit none
 integer :: i1,j1,i,j
 double precision :: c_i1,c_j1

 mo_nucl_elec_integral = 0.d0
 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j,i1,j1,c_j1,c_i1) &
 !$OMP SHARED(mo_tot_num,ao_num,mo_coef, &
 !$OMP   mo_nucl_elec_integral, ao_nucl_elec_integral)
 do i = 1, mo_tot_num
   do j = 1, mo_tot_num
    do i1 = 1,ao_num
     c_i1 = mo_coef(i1,i)
     do j1 = 1,ao_num
       c_j1 = c_i1*mo_coef(j1,j)
       mo_nucl_elec_integral(j,i) = mo_nucl_elec_integral(j,i) + &
                                           c_j1 * ao_nucl_elec_integral(j1,i1)
     enddo
    enddo
   enddo
 enddo
 !$OMP END PARALLEL DO
END_PROVIDER

