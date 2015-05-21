 BEGIN_PROVIDER [double precision, mo_dipole_x , (mo_tot_num_align,mo_tot_num)]
&BEGIN_PROVIDER [double precision, mo_dipole_y , (mo_tot_num_align,mo_tot_num)]
&BEGIN_PROVIDER [double precision, mo_dipole_z , (mo_tot_num_align,mo_tot_num)]
 BEGIN_DOC
 ! array of the integrals of MO_i * x MO_j
 ! array of the integrals of MO_i * y MO_j
 ! array of the integrals of MO_i * z MO_j
 END_DOC
 implicit none
 integer :: i1,j1,i,j
 double precision :: c_i1,c_j1

 mo_dipole_x = 0.d0
 mo_dipole_y = 0.d0
 mo_dipole_z = 0.d0
 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j,i1,j1,c_j1,c_i1) &
 !$OMP SHARED(mo_tot_num,ao_num,mo_coef, &
 !$OMP   mo_dipole_x,mo_dipole_y,mo_dipole_z,ao_dipole_x,ao_dipole_y,ao_dipole_z)
 do i = 1, mo_tot_num
   do j = 1, mo_tot_num
    do i1 = 1,ao_num
     c_i1 = mo_coef(i1,i)
     do j1 = 1,ao_num
       c_j1 = c_i1*mo_coef(j1,j)
       mo_dipole_x(j,i) = mo_dipole_x(j,i) + c_j1 * ao_dipole_x(j1,i1)
       mo_dipole_y(j,i) = mo_dipole_y(j,i) + c_j1 * ao_dipole_y(j1,i1)
       mo_dipole_z(j,i) = mo_dipole_z(j,i) + c_j1 * ao_dipole_z(j1,i1)
     enddo
    enddo
   enddo
 enddo
 !$OMP END PARALLEL DO
END_PROVIDER

 BEGIN_PROVIDER [double precision, mo_spread_x , (mo_tot_num_align,mo_tot_num)]
&BEGIN_PROVIDER [double precision, mo_spread_y , (mo_tot_num_align,mo_tot_num)]
&BEGIN_PROVIDER [double precision, mo_spread_z , (mo_tot_num_align,mo_tot_num)]
 BEGIN_DOC
 ! array of the integrals of MO_i * x^2 MO_j
 ! array of the integrals of MO_i * y^2 MO_j
 ! array of the integrals of MO_i * z^2 MO_j
 END_DOC
 implicit none
 integer :: i1,j1,i,j
 double precision :: c_i1,c_j1

 mo_nucl_elec_integral = 0.d0
 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j,i1,j1,c_j1,c_i1) &
 !$OMP SHARED(mo_tot_num,ao_num,mo_coef, &
 !$OMP   mo_spread_x,mo_spread_y,mo_spread_z,ao_spread_x,ao_spread_y,ao_spread_z)
 do i = 1, mo_tot_num
   do j = 1, mo_tot_num
    do i1 = 1,ao_num
     c_i1 = mo_coef(i1,i)
     do j1 = 1,ao_num
       c_j1 = c_i1*mo_coef(j1,j)
       mo_spread_x(j,i) = mo_spread_x(j,i) + c_j1 * ao_spread_x(j1,i1)
       mo_spread_y(j,i) = mo_spread_y(j,i) + c_j1 * ao_spread_y(j1,i1)
       mo_spread_z(j,i) = mo_spread_z(j,i) + c_j1 * ao_spread_z(j1,i1)
     enddo
    enddo
   enddo
 enddo
 !$OMP END PARALLEL DO
END_PROVIDER

 BEGIN_PROVIDER [double precision, mo_deriv_1_x , (mo_tot_num_align,mo_tot_num)]
&BEGIN_PROVIDER [double precision, mo_deriv_1_y , (mo_tot_num_align,mo_tot_num)]
&BEGIN_PROVIDER [double precision, mo_deriv_1_z , (mo_tot_num_align,mo_tot_num)]
 BEGIN_DOC
 ! array of the integrals of MO_i * d/dx  MO_j
 ! array of the integrals of MO_i * d/dy  MO_j
 ! array of the integrals of MO_i * d/dz  MO_j
 END_DOC
 implicit none
 integer :: i1,j1,i,j
 double precision :: c_i1,c_j1

 mo_nucl_elec_integral = 0.d0
 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j,i1,j1,c_j1,c_i1) &
 !$OMP SHARED(mo_tot_num,ao_num,mo_coef, &
 !$OMP   mo_deriv_1_x,mo_deriv_1_y,mo_deriv_1_z,ao_spread_x,ao_spread_y,ao_spread_z)
 do i = 1, mo_tot_num
   do j = 1, mo_tot_num
    do i1 = 1,ao_num
     c_i1 = mo_coef(i1,i)
     do j1 = 1,ao_num
       c_j1 = c_i1*mo_coef(j1,j)
       mo_deriv_1_x(j,i) = mo_deriv_1_x(j,i) + c_j1 * ao_spread_x(j1,i1)
       mo_deriv_1_y(j,i) = mo_deriv_1_y(j,i) + c_j1 * ao_spread_y(j1,i1)
       mo_deriv_1_z(j,i) = mo_deriv_1_z(j,i) + c_j1 * ao_spread_z(j1,i1)
     enddo
    enddo
   enddo
 enddo
 !$OMP END PARALLEL DO
END_PROVIDER

