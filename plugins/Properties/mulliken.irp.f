
BEGIN_PROVIDER [double precision, spin_population, (ao_num_align,ao_num)]
 implicit none
 integer :: i,j
 BEGIN_DOC
! spin population on the ao basis :
! spin_population(i,j) = rho_AO(alpha)(i,j) - rho_AO(beta)(i,j) * <AO_i|AO_j>
 END_DOC
 spin_population = 0.d0
 do i = 1, ao_num
  do j = 1, ao_num
   spin_population(j,i) = one_body_spin_density_ao(i,j) * ao_overlap(i,j)
  enddo
 enddo
END_PROVIDER

 BEGIN_PROVIDER [double precision, spin_population_angular_momentum, (0:ao_l_max)]
&BEGIN_PROVIDER [double precision, spin_population_angular_momentum_per_atom, (0:ao_l_max,nucl_num)]
 implicit none
 integer :: i
 double precision :: accu
 spin_population_angular_momentum = 0.d0
 spin_population_angular_momentum_per_atom = 0.d0
 do i = 1,  ao_num
  spin_population_angular_momentum(ao_l(i)) += spin_gross_orbital_product(i)
  spin_population_angular_momentum_per_atom(ao_l(i),ao_nucl(i)) += spin_gross_orbital_product(i)
 enddo

END_PROVIDER 


BEGIN_PROVIDER [double precision, spin_gross_orbital_product, (ao_num)]
 implicit none
 spin_gross_orbital_product = 0.d0
 integer :: i,j
 BEGIN_DOC
! gross orbital product for the spin population
 END_DOC
 do i = 1, ao_num
  do j = 1, ao_num
   spin_gross_orbital_product(i) += spin_population(j,i)
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [double precision, mulliken_spin_densities, (nucl_num)]
 implicit none
 integer :: i,j
 BEGIN_DOC
!ATOMIC SPIN POPULATION (ALPHA MINUS BETA)
 END_DOC
 mulliken_spin_densities = 0.d0
 do i = 1, ao_num
  mulliken_spin_densities(ao_nucl(i)) += spin_gross_orbital_product(i)
 enddo

END_PROVIDER

 BEGIN_PROVIDER [double precision, electronic_population_alpha, (ao_num_align,ao_num)]
&BEGIN_PROVIDER [double precision, electronic_population_beta, (ao_num_align,ao_num)]
 implicit none
 integer :: i,j
 BEGIN_DOC
! spin population on the ao basis :
! spin_population(i,j) = rho_AO(alpha)(i,j) - rho_AO(beta)(i,j) * <AO_i|AO_j>
 END_DOC
 electronic_population_alpha = 0.d0
 electronic_population_beta = 0.d0
 do i = 1, ao_num
  do j = 1, ao_num
   electronic_population_alpha(j,i) = one_body_dm_ao_alpha(i,j) * ao_overlap(i,j)
   electronic_population_beta(j,i) = one_body_dm_ao_beta(i,j) * ao_overlap(i,j)
  enddo
 enddo

END_PROVIDER

 BEGIN_PROVIDER [double precision, gross_orbital_product_alpha, (ao_num)]
&BEGIN_PROVIDER [double precision, gross_orbital_product_beta, (ao_num)]
 implicit none
 spin_gross_orbital_product = 0.d0
 integer :: i,j
 BEGIN_DOC
! gross orbital product 
 END_DOC
 do i = 1, ao_num
  do j = 1, ao_num
   gross_orbital_product_alpha(i) += electronic_population_alpha(j,i)
   gross_orbital_product_beta(i) += electronic_population_beta(j,i)
  enddo
 enddo

END_PROVIDER

 BEGIN_PROVIDER [double precision, mulliken_densities_alpha, (nucl_num)]
&BEGIN_PROVIDER [double precision, mulliken_densities_beta, (nucl_num)]
 implicit none
 integer :: i,j
 BEGIN_DOC
!
 END_DOC
 mulliken_densities_alpha = 0.d0
 mulliken_densities_beta = 0.d0
 do i = 1, ao_num
  mulliken_densities_alpha(ao_nucl(i)) += gross_orbital_product_alpha(i)
  mulliken_densities_beta(ao_nucl(i)) += gross_orbital_product_beta(i)
 enddo

END_PROVIDER


subroutine print_mulliken_sd
 implicit none
 double precision :: accu
 integer :: i
 integer :: j
 print*,'Mulliken spin densities'
 accu= 0.d0
 do i = 1, nucl_num
  print*,i,nucl_charge(i),mulliken_spin_densities(i)
  accu += mulliken_spin_densities(i)
 enddo
 print*,'Sum of Mulliken SD = ',accu
 print*,'AO SPIN POPULATIONS'
 accu = 0.d0
 do i = 1, ao_num
  accu += spin_gross_orbital_product(i)
  write(*,'(1X,I3,1X,A4,1X,I2,1X,A4,1X,F10.7)')i,trim(element_name(int(nucl_charge(ao_nucl(i))))),ao_nucl(i),trim(l_to_charater(ao_l(i))),spin_gross_orbital_product(i)
 enddo
 print*,'sum = ',accu
 accu = 0.d0
 print*,'Angular momentum analysis'
 do i = 0,  ao_l_max
  accu += spin_population_angular_momentum(i)
  print*,' ',trim(l_to_charater(i)),spin_population_angular_momentum(i)
 print*,'sum = ',accu
 enddo
 print*,'Angular momentum analysis per atom'
 print*,'Angular momentum analysis'
 do j = 1,nucl_num
  accu = 0.d0
  do i = 0,  ao_l_max
   accu += spin_population_angular_momentum_per_atom(i,j)
   write(*,'(1X,I3,1X,A4,1X,A4,1X,F10.7)')j,trim(element_name(int(nucl_charge(j)))),trim(l_to_charater(i)),spin_population_angular_momentum_per_atom(i,j)
   print*,'sum = ',accu
  enddo
 enddo

end

