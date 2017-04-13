BEGIN_PROVIDER [double precision, spin_density_at_nucleous, (nucl_num)]
 implicit none
 BEGIN_DOC
! value of the spin density at each nucleus 
 END_DOC
 integer :: i,j,k
 do i = 1, nucl_num 
  double precision :: r(3),accu,aos_array(ao_num)
  accu = 0.d0
  r(1:3) = nucl_coord(i,1:3)
  call give_all_aos_at_r(r,aos_array)
  do j = 1, ao_num
   do k = 1, ao_num
    accu += one_body_spin_density_ao(k,j) * aos_array(k) * aos_array(j)
   enddo
  enddo
  spin_density_at_nucleous(i) = accu
 enddo
END_PROVIDER

 BEGIN_PROVIDER [double precision, spin_density_at_nucleous_from_mo, (nucl_num)]
&BEGIN_PROVIDER [double precision, spin_density_at_nucleous_contrib_per_mo, (nucl_num,mo_tot_num)]
 implicit none
 BEGIN_DOC
! value of the spin density at each nucleus 
 END_DOC
 integer :: i,j,k,l,m
 do i = 1, nucl_num 
  double precision :: r(3),accu,aos_array(ao_num)
  double precision :: contrib
  double precision :: mo_values(mo_tot_num)
  accu = 0.d0
  r(1:3) = nucl_coord(i,1:3)
  call give_all_aos_at_r(r,aos_array)
  spin_density_at_nucleous_from_mo(i) = 0.d0
  do k = 1, mo_tot_num
   mo_values(k) = 0.d0
   do j = 1, ao_num
    mo_values(k) += mo_coef(j,k) * aos_array(j)
   enddo
  enddo
  do k = 1, mo_tot_num
   spin_density_at_nucleous_contrib_per_mo(i,k) = 0.d0
   do m = 1, mo_tot_num
    contrib = one_body_spin_density_mo(k,m) * mo_values(k) * mo_values(m)
    spin_density_at_nucleous_from_mo(i) += contrib
    spin_density_at_nucleous_contrib_per_mo(i,k) += contrib
   enddo
  enddo
 enddo
END_PROVIDER

 BEGIN_PROVIDER [double precision, spin_density_at_nucleous_contrib_mo, (nucl_num,mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [double precision, spin_density_at_nucleous_contrib_mo_test, (nucl_num)]
 implicit none
 BEGIN_DOC
! value of the spin density at each nucleus 
 END_DOC
 integer :: i,j,k,l,m
 spin_density_at_nucleous_contrib_mo_test = 0.d0
 do i = 1, nucl_num 
  double precision :: r(3),accu,aos_array(ao_num)
  double precision :: c_i1,c_j1  
  r(1:3) = nucl_coord(i,1:3)
  call give_all_aos_at_r(r,aos_array)
  do k = 1, mo_tot_num
   do m = 1, mo_tot_num
    accu = 0.d0
       do j = 1, ao_num
        c_i1 = mo_coef(j,k)
        do l = 1, ao_num
         c_j1 = c_i1*mo_coef(l,m)
         accu += one_body_spin_density_mo(k,m) * aos_array(l) * aos_array(j) * c_j1
        enddo
       enddo
    spin_density_at_nucleous_contrib_mo(i,k,m) = accu
    spin_density_at_nucleous_contrib_mo_test(i) += accu
   enddo 
  enddo
 enddo
END_PROVIDER

 BEGIN_PROVIDER [double precision, conversion_factor_mhz_hcc, (100)]
&BEGIN_PROVIDER [double precision, conversion_factor_gauss_hcc, (100)]
&BEGIN_PROVIDER [double precision, conversion_factor_cm_1_hcc, (100)]
 BEGIN_DOC
! Conversion factor for the calculation of the hcc, according to the nuclear charge 
 END_DOC

 conversion_factor_mhz_hcc =0.d0 
 conversion_factor_mhz_hcc =0.d0 
 conversion_factor_mhz_hcc =0.d0 


 ! hydrogen 
 conversion_factor_mhz_hcc(1) = 4469.84692227102460d0
 conversion_factor_gauss_hcc(1) = 1594.95296390862904d0
 conversion_factor_cm_1_hcc(1) = 1490.98044430157870d0

 ! Li       
 conversion_factor_mhz_hcc(3) = 1737.2746512855997d0
 conversion_factor_gauss_hcc(3) = 619.9027742370165d0
 conversion_factor_cm_1_hcc(3) = 579.4924475562677d0

 ! boron 
 conversion_factor_mhz_hcc(5) = 1434.3655101868d0
 conversion_factor_gauss_hcc(5) = 511.817264334d0   
 conversion_factor_cm_1_hcc(5) = 478.4528336953d0     

 ! carbon
 conversion_factor_mhz_hcc(6) = 1124.18303629792945d0
 conversion_factor_gauss_hcc(6) = 401.136570647523058d0
 conversion_factor_cm_1_hcc(6) = 374.987097339830086d0

 ! nitrogen
 conversion_factor_mhz_hcc(7) = 323.102093833793390d0
 conversion_factor_gauss_hcc(7) = 115.290892768082614d0
 conversion_factor_cm_1_hcc(7) = 107.775257586297698d0

 ! Oxygen
 conversion_factor_mhz_hcc(8) = -606.1958551736545d0
 conversion_factor_gauss_hcc(8) = -216.30574771560407d0
 conversion_factor_cm_1_hcc(8) = -202.20517197179822d0

 ! Phosphore
 conversion_factor_mhz_hcc(15) = 1811.0967763744873d0
 conversion_factor_gauss_hcc(15) = 646.2445276897648d0
 conversion_factor_cm_1_hcc(15) = 604.1170297381395d0
 
END_PROVIDER 

 BEGIN_PROVIDER [double precision, iso_hcc_mhz, (nucl_num)]
&BEGIN_PROVIDER [double precision, iso_hcc_gauss, (nucl_num)]
&BEGIN_PROVIDER [double precision, iso_hcc_cm_1, (nucl_num)]
 BEGIN_DOC
! isotropic hyperfine coupling constants among the various atoms
 END_DOC
 integer :: i
 do i = 1, nucl_num
   iso_hcc_mhz(i)   = conversion_factor_mhz_hcc(nint(nucl_charge(i))) * spin_density_at_nucleous(i)  !* 0.5d0
   iso_hcc_gauss(i) = conversion_factor_gauss_hcc(nint(nucl_charge(i))) * spin_density_at_nucleous(i)!* 0.5d0
   iso_hcc_cm_1(i)  = conversion_factor_cm_1_hcc(nint(nucl_charge(i))) * spin_density_at_nucleous(i) !*0.5d0
 enddo

END_PROVIDER 


subroutine print_hcc
 implicit none
 double precision :: accu
 integer :: i,j
 print*,'Z               AU           GAUSS              MHZ             cm^-1'
 do i = 1, nucl_num
  write(*,'(I2,1X,F4.1,1X,4(F16.6,1X))')i,nucl_charge(i),spin_density_at_nucleous(i),iso_hcc_gauss(i),iso_hcc_mhz(i),iso_hcc_cm_1(i)
 enddo

end

