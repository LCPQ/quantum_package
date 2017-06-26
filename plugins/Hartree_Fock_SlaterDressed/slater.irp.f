BEGIN_PROVIDER [ double precision, slater_expo, (nucl_num) ]
 implicit none
 BEGIN_DOC
 ! Exponents of the Slater functions
 END_DOC
 logical :: exists
 call ezfio_has_Hartree_Fock_SlaterDressed_slater_expo_ezfio(exists)
 if (exists) then
   slater_expo(1:nucl_num) = slater_expo_ezfio(1:nucl_num)
 else
   integer :: i
   do i=1,nucl_num
    slater_expo(i) = nucl_charge(i) 
   enddo
   call ezfio_set_Hartree_Fock_SlaterDressed_slater_expo_ezfio(slater_expo)
 endif
END_PROVIDER


BEGIN_PROVIDER [ double precision, slater_coef, (nucl_num,mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! Exponents of the Slater functions
 END_DOC
 logical :: exists
 slater_coef = 0.d0
 call ezfio_has_Hartree_Fock_SlaterDressed_slater_coef_ezfio(exists)
 if (exists) then
   slater_coef = slater_coef_ezfio
 else
   call ezfio_set_Hartree_Fock_SlaterDressed_slater_coef_ezfio(slater_coef)
 endif
END_PROVIDER

BEGIN_PROVIDER [ double precision, slater_normalization, (nucl_num) ]
 implicit none
 BEGIN_DOC
 ! Normalization of Slater functions : sqrt(expo^3/pi)
 END_DOC
 integer :: i
 do i=1,nucl_num
   slater_normalization(i) = dsqrt( slater_expo(i)**3/dacos(-1.d0) )
 enddo

END_PROVIDER

