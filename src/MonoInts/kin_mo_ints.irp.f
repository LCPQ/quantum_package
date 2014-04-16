 BEGIN_PROVIDER [double precision, mo_kinetic_integral, (mo_tot_num_align,mo_tot_num)]
 implicit none
 integer :: i1,j1,i,j
 double precision :: c_i1

 mo_kinetic_integral = 0.d0
 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j,i1,j1,c_i1) &
 !$OMP SHARED(mo_tot_num,ao_num,mo_coef,ao_Kinetic_integral, &
 !$OMP   mo_kinetic_integral)
 do i = 1,mo_tot_num
   do j = 1,mo_tot_num
    do i1 = 1,ao_num
     c_i1 = mo_coef(i1,i)
     !DIR$ VECTOR ALIGNED
     do j1 = 1,ao_num
       mo_kinetic_integral(j,i) = mo_kinetic_integral(j,i) + c_i1*mo_coef(j1,j) *&
                                  ao_Kinetic_integral(j1,i1)
     enddo
    enddo
   enddo
 enddo
 !$OMP END PARALLEL DO
END_PROVIDER

